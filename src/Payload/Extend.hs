{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Payload.Extend
  ( ExtendWith(..)
  , NamedExtension(..)
  , zipExtended
  , mergeAesonValues
  , extendDeclareNamedSchemas
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson as Aeson
import           Data.Bifunctor
import           Data.Coerce
import qualified Data.Generics as Syb
#if MIN_VERSION_openapi3(3,2,5)
import qualified Data.HashMap.Strict.InsOrd.Compat as IOHM
#else
import qualified Data.HashMap.Strict.InsOrd as IOHM
#endif
import qualified Data.Map.Strict as Map
import qualified Data.OpenApi as OpenApi
import qualified Data.OpenApi.Declare as OpenApi
import           Data.Proxy
import qualified Data.Semialign as SA
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable
import           Data.Tuple
import qualified Data.Vector as V
import           GHC.Generics
import           GHC.TypeLits

-- | The result of joining a base type with some extension. The 'ToJSON', 'FromJSON',
-- and 'ToSchema' instances present this type as a single object resulting from
-- merging the JSON objects field-wise.
data base `ExtendWith` ext = !base `ExtendWith` !ext
  deriving (Show, Read, Eq, Ord, Functor, Generic)
  deriving anyclass (NFData)
infixr 6 `ExtendWith`

instance Bifunctor ExtendWith where
  first f (a `ExtendWith` ext) = f a `ExtendWith` ext
  second = fmap

-- | Allows for specifying a name to be used for the OpenApi schema declaration.
newtype NamedExtension (name :: Symbol) base ext = NamedExtension (base `ExtendWith` ext)
  deriving (Show, Read, Eq, Ord)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON, Functor, NFData)

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
    extendDeclareNamedSchemas Nothing
      (OpenApi.declareNamedSchema (Proxy @ext))
      (OpenApi.declareNamedSchema (Proxy @base))

instance (OpenApi.ToSchema ext, OpenApi.ToSchema base, KnownSymbol name) => OpenApi.ToSchema (NamedExtension name base ext) where
  declareNamedSchema _ =
    extendDeclareNamedSchemas (Just . T.pack $ symbolVal (Proxy @name))
      (OpenApi.declareNamedSchema (Proxy @ext))
      (OpenApi.declareNamedSchema (Proxy @base))

extendDeclareNamedSchemas
  :: Maybe Text
  -> OpenApi.Declare (OpenApi.Definitions OpenApi.Schema) OpenApi.NamedSchema
  -> OpenApi.Declare (OpenApi.Definitions OpenApi.Schema) OpenApi.NamedSchema
  -> OpenApi.Declare (OpenApi.Definitions OpenApi.Schema) OpenApi.NamedSchema
extendDeclareNamedSchemas mName declareExt declareBase = do
  let (extRefs, namedExtSchema) = OpenApi.runDeclare declareExt mempty
      (baseRefs, baseSchema) = OpenApi.runDeclare declareBase mempty
      mDefaultName = do
        baseName <- OpenApi._namedSchemaName baseSchema
        extName <- OpenApi._namedSchemaName namedExtSchema
        Just $ baseName <> "+" <> extName
      (mergedSchema, mergedRefs) =
        extendOpenApi
          mName
          extRefs
          (OpenApi._namedSchemaSchema namedExtSchema)
          baseRefs
          (OpenApi._namedSchemaSchema baseSchema)
  OpenApi.declare mergedRefs
  pure namedExtSchema
    { OpenApi._namedSchemaSchema = mergedSchema
    , OpenApi._namedSchemaName = mName <|> mDefaultName
    }

extendOpenApi
  :: Maybe Text
  -> OpenApi.Definitions OpenApi.Schema
  -> OpenApi.Schema
  -> OpenApi.Definitions OpenApi.Schema
  -> OpenApi.Schema
  -> (OpenApi.Schema, OpenApi.Definitions OpenApi.Schema)
extendOpenApi mSchemaName extRefs extSchema baseRefs baseSchema =
  let resolveRefs (OpenApi.Inline xS) (OpenApi.Inline bS) =
        OpenApi.Inline <$> merge Nothing xS bS
      -- This uses the ref name from the base schema for the resulting merged schema.
      -- Should make a hybrid of them so that there are no unintended overrides
      resolveRefs (OpenApi.Ref xRef) (OpenApi.Ref bRef) = do
        let mergedRef = coerce (coerce bRef <> "+" <> coerce xRef :: Text)
        gets (IOHM.lookup $ coerce mergedRef) >>= \case
          Just _ -> pure (OpenApi.Ref mergedRef) -- ref already processed
          Nothing -> do
            let mXS = IOHM.lookup (coerce xRef) extRefs
                mBS = IOHM.lookup (coerce bRef) baseRefs
            case (mXS, mBS) of
              (Just xS, Just bS) -> do
                merged <- merge Nothing xS bS
                modify (IOHM.insert (coerce mergedRef) merged)
                pure $ OpenApi.Ref mergedRef
              _ -> pure $ OpenApi.Ref bRef -- shouldn't happen
      resolveRefs (OpenApi.Ref xRef) (OpenApi.Inline bS) =
        case IOHM.lookup (coerce xRef) extRefs of
          Nothing -> pure (OpenApi.Inline bS) -- shouldn't happen
          Just xS -> OpenApi.Inline <$> merge Nothing xS bS
      resolveRefs (OpenApi.Inline xS) (OpenApi.Ref bRef) =
        case IOHM.lookup (coerce bRef) baseRefs of
          Nothing -> pure (OpenApi.Ref bRef) -- shouldn't happen
          Just bS -> OpenApi.Inline <$> merge Nothing xS bS

      merge
        :: Maybe Text
        -> OpenApi.Schema
        -> OpenApi.Schema
        -> State (OpenApi.Definitions OpenApi.Schema) OpenApi.Schema
      merge mTitle xS bS = do
        -- There's some hoop jumping due to the annoying monomorphic types of
        -- the compat wrapper. Thankfully they did not make 'mapWithKey' monomorphic.
        let xSProps = OpenApi._schemaProperties xS
            xSReqFields = Set.fromList $ OpenApi._schemaRequired xS
            bSProps = OpenApi._schemaProperties bS
            bSReqFields = Set.fromList $ OpenApi._schemaRequired bS

#if MIN_VERSION_openapi3(3,2,5)
            xSPropsA = IOHM.mapWithKey (\_ v -> pure v) xSProps
            bSPropsA = IOHM.mapWithKey (\_ v -> pure v) bSProps
            mergedPropsA = IOHM.intersectionWith
                      (\ma mb -> ma >>= \a -> mb >>= \b ->
                        resolveRefs a b
                      )
                      xSPropsA
                      bSPropsA
#else
            mergedPropsA = IOHM.intersectionWith
                      resolveRefs
                      xSProps
                      bSProps
#endif
        mergedProps <- sequenceA mergedPropsA
        let mXSItems = OpenApi._schemaItems xS
            mBSItems = OpenApi._schemaItems bS
        mMergedItems <- case (mXSItems, mBSItems) of
          (Just (OpenApi.OpenApiItemsObject xIS), Just (OpenApi.OpenApiItemsObject bIS))
            -> Just . OpenApi.OpenApiItemsObject <$> resolveRefs xIS bIS
          (Just (OpenApi.OpenApiItemsArray xIS), Just (OpenApi.OpenApiItemsArray bIS))
            -> Just . OpenApi.OpenApiItemsArray <$> zipWithM resolveRefs xIS bIS
          _ -> pure mXSItems
        mMergedOneOf :: Maybe (Map.Map Text Text, [OpenApi.Referenced OpenApi.Schema]) <-
          fmap (<|> ((mempty,) <$> OpenApi._schemaOneOf xS))
          . runMaybeT
          $ do
              xOneOf <- hoistMaybe $ OpenApi._schemaOneOf xS
              OpenApi.Discriminator _ bMap <- hoistMaybe $ OpenApi._schemaDiscriminator bS
              OpenApi.Discriminator _ xMap <- hoistMaybe $ OpenApi._schemaDiscriminator xS
              let xMapSwap = Map.fromList $ swap <$> IOHM.toList xMap
              fmap sequence . for xOneOf $ \case
                OpenApi.Ref (OpenApi.Reference xRef)
                  | Just key <- Map.lookup xRef xMapSwap
                  , Just bRef <- IOHM.lookup key bMap
                  -> do
                    OpenApi.Ref newRef <- lift $ resolveRefs
                      (OpenApi.Ref (OpenApi.Reference xRef))
                      (OpenApi.Ref (OpenApi.Reference bRef))
                    pure (Map.singleton xRef (coerce newRef), OpenApi.Ref newRef)
                x -> pure (mempty, x)
        let mCombinedTitle = do
              xTitle <- OpenApi._schemaTitle xS
              bTitle <- OpenApi._schemaTitle bS
              pure $ bTitle <> "+" <> xTitle
        pure xS
          { OpenApi._schemaProperties = mergedProps <> xSProps <> bSProps
          , OpenApi._schemaRequired =
              Set.toList $ xSReqFields <> bSReqFields
          , OpenApi._schemaExample =
              mergeAesonValues
                <$> OpenApi._schemaExample xS
                <*> OpenApi._schemaExample bS
          , OpenApi._schemaItems = mMergedItems
          , OpenApi._schemaDescription = OpenApi._schemaDescription xS <|> OpenApi._schemaDescription bS
          , OpenApi._schemaTitle = mTitle <|> mCombinedTitle <|> OpenApi._schemaTitle xS <|> OpenApi._schemaTitle bS
          , OpenApi._schemaXml = OpenApi._schemaXml xS <|> OpenApi._schemaXml bS
          , OpenApi._schemaExternalDocs = OpenApi._schemaExternalDocs xS <|> OpenApi._schemaExternalDocs bS
          , OpenApi._schemaDiscriminator = do
              OpenApi.Discriminator k xMap <- OpenApi._schemaDiscriminator xS
              (mergeMapping, _) <- mMergedOneOf
              let mergedMap = IOHM.map (\key -> Map.findWithDefault key key mergeMapping) xMap
              pure $ OpenApi.Discriminator k mergedMap
          , OpenApi._schemaOneOf = snd <$> mMergedOneOf
          }

      (mergedSchema, mergedRefs) =
        runState (merge mSchemaName extSchema baseSchema) mempty
      -- Get all the schema references from the merged result so that schemas
      -- that are no longer references can be dropped from the reference map.
      remainingRefs =
        Syb.everything (<>)
          (Syb.mkQ Set.empty (\(OpenApi.Reference ref) -> Set.singleton ref))
          (mergedSchema, mergedRefs)

  in ( mergedSchema
     , mergedRefs
        <> IOHM.filterWithKey (\k _ -> Set.member k remainingRefs) extRefs
        <> IOHM.filterWithKey (\k _ -> Set.member k remainingRefs) baseRefs
     )
