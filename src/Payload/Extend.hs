{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
module Payload.Extend
  ( ExtendedWith(..)
  , zipExtended
  , mergeAesonValues
  , extendDeclareNamedSchemas
  ) where

import           Control.Monad
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
import           Control.Monad.Trans.Writer.CPS
import           Control.Monad.State

-- | The result of joining a base type with some extension. The 'ToJSON', 'FromJSON',
-- and 'ToSchema' instances present this type as a single object resulting from
-- merging the JSON objects field-wise.
data a `ExtendedWith` b = !a `ExtendedWith` !b
  deriving (Show, Eq, Ord, Functor)

instance Bifunctor ExtendedWith where
  first f (a `ExtendedWith` ext) = f a `ExtendedWith` ext
  second = fmap

zipExtended :: SA.Zip f => f a `ExtendedWith` f b -> f (a `ExtendedWith` b)
zipExtended (fa `ExtendedWith` fb) = SA.zipWith ExtendedWith fa fb

instance (Aeson.ToJSON ext, Aeson.ToJSON base) => Aeson.ToJSON (base `ExtendedWith` ext) where
  toJSON (base `ExtendedWith` ext) = mergeAesonValues (Aeson.toJSON ext) (Aeson.toJSON base)

mergeAesonValues :: Aeson.Value -> Aeson.Value -> Aeson.Value
mergeAesonValues = \cases
  (Aeson.Object a) (Aeson.Object b) -> Aeson.Object $ KeyMap.unionWith mergeAesonValues a b
  (Aeson.Array a) (Aeson.Array b) -> Aeson.Array (V.zipWith mergeAesonValues a b)
  a _ -> a

instance (Aeson.FromJSON ext, Aeson.FromJSON base) => Aeson.FromJSON (base `ExtendedWith` ext) where
  parseJSON = liftA2 ExtendedWith <$> Aeson.parseJSON <*> Aeson.parseJSON

instance (OpenApi.ToSchema ext, OpenApi.ToSchema base) => OpenApi.ToSchema (base `ExtendedWith` ext) where
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
        pure bS
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
