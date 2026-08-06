{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Payload.Extend
  ( ExtendWith(..)
  , zipExtended
  , mergeAesonValues
  , extendDeclareNamedSchemas
  , OptionalObj(..)
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Writer.CPS
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson as Aeson
import           Data.Bifunctor
import           Data.Coerce
#if MIN_VERSION_openapi3(3,2,5)
import qualified Data.HashMap.Strict.InsOrd.Compat as IOHM
#else
import qualified Data.HashMap.Strict.InsOrd as IOHM
#endif
import qualified Data.OpenApi as OpenApi
import qualified Data.OpenApi.Declare as OpenApi
import           Data.Proxy
import qualified Data.Semialign as SA
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Vector as V
import           GHC.Generics

-- | The result of joining a base type with some extension. The 'ToJSON', 'FromJSON',
-- and 'ToSchema' instances present this type as a single object resulting from
-- merging the JSON objects field-wise.
data a `ExtendWith` b = !a `ExtendWith` !b
  deriving (Show, Read, Eq, Ord, Functor, Generic)
  deriving anyclass (NFData)
infixr 6 `ExtendWith`

instance Bifunctor ExtendWith where
  first f (a `ExtendWith` ext) = f a `ExtendWith` ext
  second = fmap

zipExtended :: SA.Zip f => f a `ExtendWith` f b -> f (a `ExtendWith` b)
zipExtended (fa `ExtendWith` fb) = SA.zipWith ExtendWith fa fb

instance (Aeson.ToJSON ext, Aeson.ToJSON base) => Aeson.ToJSON (base `ExtendWith` ext) where
  toJSON (base `ExtendWith` ext) = mergeAesonValues (Aeson.toJSON ext) (Aeson.toJSON base)

mergeAesonValues :: Aeson.Value -> Aeson.Value -> Aeson.Value
mergeAesonValues = \cases
  (Aeson.Object a) (Aeson.Object b) -> Aeson.Object $ KeyMap.unionWith mergeAesonValues a b
  (Aeson.Array a) (Aeson.Array b) -> Aeson.Array (V.zipWith mergeAesonValues a b)
  a _ -> a

instance (Aeson.FromJSON ext, Aeson.FromJSON base) => Aeson.FromJSON (base `ExtendWith` ext) where
  parseJSON = liftA2 ExtendWith <$> Aeson.parseJSON <*> Aeson.parseJSON

instance (OpenApi.ToSchema ext, OpenApi.ToSchema base) => OpenApi.ToSchema (base `ExtendWith` ext) where
  declareNamedSchema _ =
    extendDeclareNamedSchemas
      (OpenApi.declareNamedSchema (Proxy @ext))
      (OpenApi.declareNamedSchema (Proxy @base))

extendDeclareNamedSchemas
  :: OpenApi.Declare (OpenApi.Definitions OpenApi.Schema) OpenApi.NamedSchema
  -> OpenApi.Declare (OpenApi.Definitions OpenApi.Schema) OpenApi.NamedSchema
  -> OpenApi.Declare (OpenApi.Definitions OpenApi.Schema) OpenApi.NamedSchema
extendDeclareNamedSchemas declareExt declareBase = do
  let (extRefs, namedExtSchema) = OpenApi.runDeclare declareExt mempty
      (baseRefs, baseSchema) = OpenApi.runDeclare declareBase mempty
      (mergedSchema, mergedRefs) =
        extendOpenApi
          extRefs
          (OpenApi._namedSchemaSchema namedExtSchema)
          baseRefs
          (OpenApi._namedSchemaSchema baseSchema)
  OpenApi.declare mergedRefs
  pure (baseSchema { OpenApi._namedSchemaSchema = mergedSchema })

extendOpenApi
  :: OpenApi.Definitions OpenApi.Schema
  -> OpenApi.Schema
  -> OpenApi.Definitions OpenApi.Schema
  -> OpenApi.Schema
  -> (OpenApi.Schema, OpenApi.Definitions OpenApi.Schema)
extendOpenApi extRefs extSchema baseRefs baseSchema =
  let resolveRefs optInherit (OpenApi.Inline xS) (OpenApi.Inline bS) =
        OpenApi.Inline <$> merge optInherit xS bS
      resolveRefs optInherit (OpenApi.Ref xRef) (OpenApi.Ref bRef) =
        gets (IOHM.lookup (coerce bRef)) >>= \case
          Just _ -> pure (OpenApi.Ref bRef) -- ref already processed
          Nothing -> do
            let mXS = IOHM.lookup (coerce xRef) extRefs
                mBS = IOHM.lookup (coerce bRef) baseRefs
            case (mXS, mBS) of
              (Just xS, Just bS) -> do
                merged <- merge optInherit xS bS
                tell (Set.singleton (coerce xRef))
                modify (IOHM.insert (coerce bRef) merged)
                pure $ OpenApi.Ref bRef
              _ -> pure $ OpenApi.Ref bRef -- shouldn't happen
      resolveRefs optInherit (OpenApi.Ref xRef) (OpenApi.Inline bS) =
        case IOHM.lookup (coerce xRef) extRefs of
          Nothing -> pure (OpenApi.Inline bS) -- shouldn't happen
          Just xS -> OpenApi.Inline <$> merge optInherit xS bS
      resolveRefs optInherit (OpenApi.Inline xS) (OpenApi.Ref bRef) =
        gets (IOHM.lookup (coerce bRef)) >>= \case
          Just _ -> pure (OpenApi.Ref bRef) -- ref already processed
          Nothing ->
            case IOHM.lookup (coerce bRef) baseRefs of
              Nothing -> pure (OpenApi.Ref bRef) -- shouldn't happen
              Just bS -> do
                merged <- merge optInherit xS bS
                modify (IOHM.insert (coerce bRef) merged)
                pure $ OpenApi.Ref bRef

      merge
        :: OptionalityInherit
        -> OpenApi.Schema
        -> OpenApi.Schema
        -> WriterT (Set Text) (State (OpenApi.Definitions OpenApi.Schema)) OpenApi.Schema
      merge optInherit xS bS = do
        -- There's some hoop jumping due to the annoying monomorphic types of
        -- the compat wrapper. Thankfully they did not make 'mapWithKey' monomorphic.
        let xSProps = OpenApi._schemaProperties xS
            xSReqFields = Set.fromList $ OpenApi._schemaRequired xS
            bSProps = OpenApi._schemaProperties bS
            bSReqFields = Set.fromList $ OpenApi._schemaRequired bS
            determineOptInherit key = case optInherit of
              NoOpt
                | xIsOpt && not bIsOpt -> OptExt
                | bIsOpt && not xIsOpt -> OptBase
              other -> other
              where
                xIsOpt = Set.notMember key xSReqFields
                bIsOpt = Set.notMember key bSReqFields

#if MIN_VERSION_openapi3(3,2,5)
            xSPropsA = IOHM.mapWithKey (\_ v -> pure v) xSProps
            bSPropsA = IOHM.mapWithKey (\_ v -> pure v) bSProps
            mergedPropsA = IOHM.intersectionWithKey
                      (\k ma mb -> ma >>= \a -> mb >>= \b ->
                        resolveRefs (determineOptInherit k) a b
                      )
                      xSPropsA
                      bSPropsA
#else
            mergedPropsA = IOHM.intersectionWithKey
                      (resolveRefs . determineOptInherit)
                      xSProps
                      bSProps
#endif
        mergedProps <- sequenceA mergedPropsA
        let mXSItems = OpenApi._schemaItems xS
            mBSItems = OpenApi._schemaItems bS
        mMergedItems <- case (mXSItems, mBSItems) of
          (Just (OpenApi.OpenApiItemsObject xIS), Just (OpenApi.OpenApiItemsObject bIS))
            -> Just . OpenApi.OpenApiItemsObject <$> resolveRefs optInherit xIS bIS
          (Just (OpenApi.OpenApiItemsArray xIS), Just (OpenApi.OpenApiItemsArray bIS))
            -> Just . OpenApi.OpenApiItemsArray <$> zipWithM (resolveRefs optInherit) xIS bIS
          _ -> pure mXSItems
        pure xS
          { OpenApi._schemaProperties = mergedProps <> xSProps <> bSProps
          , OpenApi._schemaRequired =
              -- If the extension schema is for an optional field And the base
              -- schema field is not optional, Then all extension fields in the
              -- schema should be optional Additionally, all child fields for
              -- the exension should inherit this optionality
              --
              -- Similarly, if the base field is optional but the extension
              -- field is not, then all base fields and descendents should be optional
              Set.toList $ case optInherit of
                NoOpt -> xSReqFields <> bSReqFields
                OptExt -> bSReqFields
                OptBase -> xSReqFields
          , OpenApi._schemaExample =
              mergeAesonValues
                <$> OpenApi._schemaExample xS
                <*> OpenApi._schemaExample bS
          , OpenApi._schemaItems = mMergedItems
          , OpenApi._schemaDescription = OpenApi._schemaDescription xS <|> OpenApi._schemaDescription bS
          , OpenApi._schemaTitle = OpenApi._schemaTitle xS <|> OpenApi._schemaTitle bS
          , OpenApi._schemaXml = OpenApi._schemaXml xS <|> OpenApi._schemaXml bS
          , OpenApi._schemaExternalDocs = OpenApi._schemaExternalDocs xS <|> OpenApi._schemaExternalDocs bS
          }

      ((mergedSchema, refsToRemove), mergedRefs) =
        runState (runWriterT $ merge NoOpt extSchema baseSchema) mempty

  in ( mergedSchema
     , mergedRefs
        <> IOHM.filterWithKey (\k _ -> Set.notMember k refsToRemove) extRefs
        <> baseRefs
     )

data OptionalityInherit
  = OptExt
  | OptBase
  | NoOpt

-- | A wrapper used for extension objects that are optional. From the API
-- contract perspective,
--
-- @
-- a `ExtendWith` OptionalObj Nothing == a
-- @
--
-- This allows you to have a response that contains the union of a set of
-- extensions, each corresponding to constructors of a sum type.
-- Using this type for a request body is not supported.
--
-- @
-- data Sum = A ObjA | B ObjB | C ObjC
-- data Base = Base {...}
-- data ObjAExt = ObjAExt {...}
-- data ObjBExt = ObjBExt {...}
-- data ObjCExt = ObjCExt {...}
--
-- type ResponseForA = Base `ExtendWith` ObjAExt
-- type ResponseUnion = Base
--   `ExtendWith` OptionalObj ObjAExt
--   `ExtendWith` OptionalObj ObjBExt
--   `ExtendWith` OptionalObj ObjCExt
-- @
newtype OptionalObj a = OptionalObj { unOptionalObj :: Maybe a }
  deriving stock Generic
  deriving newtype
    (Functor, Applicative, Monad, Eq, Ord, Show, Read, NFData, Alternative, Semigroup, Monoid)

instance Aeson.ToJSON a => Aeson.ToJSON (OptionalObj a) where
  toJSON (OptionalObj a) =
    case Aeson.toJSON a of
      Aeson.Null -> Aeson.object []
      o -> o

-- It's unclear how best to handle this. For now the type is encode only.
-- instance (Aeson.FromJSON a, OpenApi.ToSchema a) => Aeson.FromJSON (OptionalObj a) where
--   parseJSON v =
    --
    -- This implementation checks that the JSON value has the expected shape
    -- before committing to parsing the object. If not, then any parse failures
    -- are discarded.
    -- let (defs, schema) =
    --       OpenApi.runDeclare (OpenApi.declareSchema (Proxy @a)) mempty
    --     validationResult =
    --       OpenApi.validateJSON
    --         defs
    --         (schema & everywhere additionalProperties ?~ OpenApi.AdditionalPropertiesAllowed True)
    --         v
    -- in case validationResult of
    --   [] -> OptionalObj <$> Aeson.parseJSON v
    --   errs -> traceShow errs pure (OptionalObj Nothing)

    -- OptionalObj <$> (Aeson.parseJSON v <|> pure Nothing)

    -- let schema = OpenApi.undeclare (OpenApi.declareSchema (Proxy @(OptionalObj a)))
    --     topFields = IOHM.keys $ OpenApi._schemaProperties schema
    -- in case v of
    --     Aeson.Object o
    --       | all (\k -> not $ KeyMap.member (Key.fromText k) o) topFields ->
    --           pure $ OptionalObj Nothing
    --     _ -> OptionalObj <$> Aeson.parseJSON v

instance OpenApi.ToSchema a => OpenApi.ToSchema (OptionalObj a) where
  declareNamedSchema _ =
    (\ns -> ns
      {OpenApi._namedSchemaSchema = (OpenApi._namedSchemaSchema ns)
        { OpenApi._schemaRequired = mempty }
      }
    ) <$> OpenApi.declareNamedSchema (Proxy @(Maybe a))
