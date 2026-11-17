{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
module Payload.AtPath
  ( AtPath(..)
  , Path
  , PathField(..)
  , Optionality(..)
  , Container(..)
  , Req
  , Opt
  , Arr
  , OptArr
  , AtPathFieldTypeMay
  , AtPathFieldType
  ) where

import           Control.DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import           Data.Coerce
import qualified Data.Generics as Syb
#if MIN_VERSION_openapi3(3,2,5)
import qualified Data.HashMap.Strict.InsOrd.Compat as IOHM
#else
import qualified Data.HashMap.Strict.InsOrd as IOHM
#endif
import           Data.Kind
import qualified Data.List as List
import qualified Data.OpenApi as OpenApi
import           Data.Proxy
import qualified Data.Text as T
import           GHC.Records
import           GHC.TypeLits

type Path = [PathField]

type data Optionality
  = Optional -- | Optional field that also allows null
  | Nullable
  | Required

type data Container
  = NonArray
  | Array

type data PathField = Field Optionality Container Symbol

-- Convenience synonyms
type Req sym = Field Required NonArray sym
type Opt sym = Field Optional NonArray sym
type Arr sym = Field Required Array sym
type OptArr sym = Field Optional Array sym

-- | Allows an extension object to be merged at a specific path in the base
-- object. This offers a surgical approach to payload extension where deeply
-- nested extension records can be defined independent of the structure of the
-- base type.
newtype AtPath a (path :: Path) = AtPath a
  deriving (Show, Read, Eq, Ord)
  deriving newtype (NFData)

newtype Validated a = Validated a

type Validate :: Type -> Constraint
type family Validate atPath where
  Validate (AtPath (Maybe a) (Field Optional c sym ': p)) = ()

  Validate (AtPath (Maybe a) (Field Nullable c sym ': p)) = ()

  Validate (AtPath (Maybe a) p)
    = TypeError (Text "Maybe values using AtPath must be declared as Optional or Nullable")

  Validate (AtPath (f a) (Field Required Array sym ': p)) = ()

  Validate (AtPath a (Field Required NonArray sym ': p)) = ()

  Validate (AtPath a (Field opt c sym ': p))
    = TypeError (Text "Invalid JSON path component at key '" :<>: Text sym :<>: Text "'")

--------------------------------------------------------------------------------
-- ToJSON
--------------------------------------------------------------------------------

instance Aeson.ToJSON a => Aeson.ToJSON (AtPath a '[]) where
  toJSON = Aeson.toJSON @a . coerce

instance
  ( Validate (AtPath a (p ': ps))
  , Aeson.ToJSON (Validated (AtPath a (p ': ps)))
  )
  => Aeson.ToJSON (AtPath a (p ': ps)) where
  toJSON = Aeson.toJSON . Validated

instance Aeson.ToJSON (AtPath a (Field Required c sym ': p))
  => Aeson.ToJSON (Validated (AtPath (Maybe a) (Field Optional c sym ': p))) where
  toJSON
    = maybe
        (Aeson.object [])
        (Aeson.toJSON @(AtPath a (Field Required c sym ': p)))
    . coerce

instance
  ( Aeson.ToJSON (AtPath a (Field Required c sym ': p))
  , KnownSymbol sym
  ) => Aeson.ToJSON (Validated (AtPath (Maybe a) (Field Nullable c sym ': p))) where
  toJSON
    = maybe
        (Aeson.object [Key.fromString (symbolVal (Proxy @sym)) Aeson..= Aeson.Null])
        (Aeson.toJSON @(AtPath a (Field Required c sym ': p)))
    . coerce

instance
  ( Aeson.ToJSON (AtPath a p)
  , KnownSymbol sym
  , Functor f
  , Aeson.ToJSON (f Aeson.Value)
  , Coercible (f a) (f (AtPath a p))
  ) => Aeson.ToJSON (Validated (AtPath (f a) (Field Required Array sym ': p))) where
  toJSON x =
    Aeson.object
      [ Key.fromString (symbolVal (Proxy @sym))
          Aeson..= fmap Aeson.toJSON (coerce @_ @(f (AtPath a p)) x)
      ]

instance
  ( Aeson.ToJSON (AtPath a p)
  , KnownSymbol sym
  ) => Aeson.ToJSON (Validated (AtPath a (Field Required NonArray sym ': p))) where
  toJSON x =
    Aeson.object
      [ Key.fromString (symbolVal (Proxy @sym))
          Aeson..= coerce @_ @(AtPath a p) x
      ]

--------------------------------------------------------------------------------
-- FromJSON
--------------------------------------------------------------------------------

instance Aeson.FromJSON a => Aeson.FromJSON (AtPath a '[]) where
  parseJSON = coerce . Aeson.parseJSON @a

instance
  ( Validate (AtPath a (p ': ps))
  , Aeson.FromJSON (Validated (AtPath a (p ': ps)))
  )
  => Aeson.FromJSON (AtPath a (p ': ps)) where
  parseJSON = fmap (coerce @(Validated (AtPath a (p ': ps)))) . Aeson.parseJSON

instance
  ( Aeson.FromJSON (AtPath a (Field Required c sym ': p))
  , KnownSymbol sym
  ) => Aeson.FromJSON (Validated (AtPath (Maybe a) (Field Optional c sym ': p))) where
  parseJSON
    = Aeson.withObject "AtPath" $ \obj ->
      case KeyMap.lookup (Key.fromString (symbolVal (Proxy @sym))) obj of
        Nothing -> pure $ coerce (Nothing @a)
        Just Aeson.Null -> pure $ coerce (Nothing @a)
        Just _ -> coerce . Just
          <$> Aeson.parseJSON @(AtPath a (Field Required c sym ': p)) (Aeson.Object obj)

instance
  ( Aeson.FromJSON (AtPath a (Field Required c sym ': p))
  , KnownSymbol sym
  ) => Aeson.FromJSON (Validated (AtPath (Maybe a) (Field Nullable c sym ': p))) where
  parseJSON
    = Aeson.withObject "AtPath" $ \obj -> do
        x <- obj Aeson..: Key.fromString (symbolVal (Proxy @sym))
        case x of
          Aeson.Null -> pure $ coerce (Nothing @a)
          _ ->
            coerce . Just
              <$> Aeson.parseJSON @(AtPath a (Field Required c sym ': p))
                    (Aeson.Object obj)

instance
  ( Aeson.FromJSON (AtPath a p)
  , KnownSymbol sym
  , Aeson.FromJSON (f (AtPath a p))
  , Coercible (f a) (f (AtPath a p))
  ) => Aeson.FromJSON (Validated (AtPath (f a) (Field Required Array sym ': p))) where
  parseJSON = Aeson.withObject "AtPath" $ \obj ->
    coerce $
      Aeson.parseField @(f (AtPath a p))
        obj
        (Key.fromString (symbolVal (Proxy @sym)))

instance
  (Aeson.FromJSON (AtPath a p), KnownSymbol sym)
  => Aeson.FromJSON (Validated (AtPath a (Field Required NonArray sym ': p))) where
  parseJSON = Aeson.withObject "AtPath" $ \obj ->
    coerce @(Aeson.Parser (AtPath a p))
      $ obj Aeson..: Key.fromString (symbolVal (Proxy @sym))

--------------------------------------------------------------------------------
-- ToSchema
--------------------------------------------------------------------------------

instance OpenApi.ToSchema a => OpenApi.ToSchema (AtPath a '[]) where
  declareNamedSchema _ = OpenApi.declareNamedSchema (Proxy @a)

instance
  ( Validate (AtPath a (p ': ps))
  , Syb.Typeable a , Syb.Typeable ps , Syb.Typeable p
  , OpenApi.ToSchema (Validated (AtPath a (p ': ps)))
  )
  => OpenApi.ToSchema (AtPath a (p ': ps)) where
  declareNamedSchema _ = OpenApi.declareNamedSchema (Proxy @(Validated (AtPath a (p ': ps))))

instance
  ( KnownSymbol sym
  , Syb.Typeable a, Syb.Typeable p, Syb.Typeable c
  , OpenApi.ToSchema (AtPath a (Field Required c sym ': p))
  ) => OpenApi.ToSchema (Validated (AtPath (Maybe a) (Field Optional c sym ': p))) where
  declareNamedSchema _ = do
    ns <- OpenApi.declareNamedSchema (Proxy @(AtPath a (Field Required c sym ': p)))
    let key = T.pack $ symbolVal (Proxy @sym)
    pure ns
      { OpenApi._namedSchemaSchema =
        let s = OpenApi._namedSchemaSchema ns in
        s { OpenApi._schemaRequired =
              List.delete key (OpenApi._schemaRequired s)
          , OpenApi._schemaProperties =
              IOHM.adjust
                (\case
                  OpenApi.Inline x -> OpenApi.Inline x { OpenApi._schemaNullable = Just True }
                  x@OpenApi.Ref{} -> x -- cannot happen
                )
                key
                (OpenApi._schemaProperties s)
          }
      }

instance
  ( KnownSymbol sym
  , Syb.Typeable a, Syb.Typeable p, Syb.Typeable c
  , OpenApi.ToSchema (AtPath a (Field Required c sym ': p))
  )
  => OpenApi.ToSchema (Validated (AtPath (Maybe a) (Field Nullable c sym ': p))) where
  declareNamedSchema _ = do
    ns <- OpenApi.declareNamedSchema (Proxy @(AtPath a (Field Required c sym ': p)))
    let key = T.pack $ symbolVal (Proxy @sym)
    pure ns
      { OpenApi._namedSchemaSchema =
        let s = OpenApi._namedSchemaSchema ns in
        s { OpenApi._schemaProperties =
              IOHM.adjust
                (\case
                  OpenApi.Inline x -> OpenApi.Inline x { OpenApi._schemaNullable = Just True }
                  x@OpenApi.Ref{} -> x -- cannot happen
                )
                key
                (OpenApi._schemaProperties s)
          }
      }

instance
  ( OpenApi.ToSchema (AtPath a p)
  , KnownSymbol sym
  , Syb.Typeable a, Syb.Typeable p
  ) => OpenApi.ToSchema (Validated (AtPath [a] (Field Required Array sym ': p))) where
  declareNamedSchema _ = do
    ns <- OpenApi.declareNamedSchema (Proxy @(AtPath a p))
    let key = T.pack $ symbolVal (Proxy @sym)
        mName = do
          n <- OpenApi._namedSchemaName ns
          Just $ key <> "[]." <> n
    pure OpenApi.NamedSchema
      { OpenApi._namedSchemaName = mName
      , OpenApi._namedSchemaSchema = mempty
        { OpenApi._schemaProperties =
            IOHM.singleton key $ OpenApi.Inline mempty
              { OpenApi._schemaItems = Just
                  $ OpenApi.OpenApiItemsObject (OpenApi.Inline $ OpenApi._namedSchemaSchema ns)
              , OpenApi._schemaType = Just OpenApi.OpenApiArray
              }
        , OpenApi._schemaRequired = [key]
        , OpenApi._schemaTitle =
            OpenApi._schemaTitle $ OpenApi._namedSchemaSchema ns
        , OpenApi._schemaType = Just OpenApi.OpenApiObject
        }
      }

instance
  ( OpenApi.ToSchema (AtPath a p)
  , KnownSymbol sym
  , Syb.Typeable a, Syb.Typeable p
  ) => OpenApi.ToSchema (Validated (AtPath a (Field Required NonArray sym ': p))) where
  declareNamedSchema _ = do
    ns <- OpenApi.declareNamedSchema (Proxy @(AtPath a p))
    let key = T.pack $ symbolVal (Proxy @sym)
        mName = do
          n <- OpenApi._namedSchemaName ns
          Just $ key <> "." <> n
    pure OpenApi.NamedSchema
      { OpenApi._namedSchemaName = mName
      , OpenApi._namedSchemaSchema = mempty
        { OpenApi._schemaProperties =
            IOHM.singleton key (OpenApi.Inline $ OpenApi._namedSchemaSchema ns)
        , OpenApi._schemaRequired = [key]
        , OpenApi._schemaTitle =
            OpenApi._schemaTitle $ OpenApi._namedSchemaSchema ns
        , OpenApi._schemaType = Just OpenApi.OpenApiObject
        }
      }

--------------------------------------------------------------------------------
-- HasField instance
--------------------------------------------------------------------------------

type AtPathFieldTypeMay :: Symbol -> Type -> Maybe Type
type family AtPathFieldTypeMay sym a where
  AtPathFieldTypeMay sym (AtPath a '[Field o c sym])
    = Just a
  AtPathFieldTypeMay sym (AtPath a (Field Required NonArray sym ': p1 ': p2))
    = Just (AtPath a (p1 ': p2))
  AtPathFieldTypeMay sym (AtPath (f a) (Field Required Array sym ': p1 ': p2))
    = Just (f (AtPath a (p1 ': p2)))
  AtPathFieldTypeMay sym (AtPath (Maybe a) (Field Optional NonArray sym ': p1 ': p2))
    = Just (Maybe (AtPath a (p1 ': p2)))
  AtPathFieldTypeMay sym (AtPath (Maybe a) (Field Nullable NonArray sym ': p1 ': p2))
    = Just (Maybe (AtPath a (p1 ': p2)))
  AtPathFieldTypeMay sym (AtPath (Maybe (f a)) (Field Optional Array sym ': p1 ': p2))
    = Just (Maybe (f (AtPath a (p1 ': p2))))
  AtPathFieldTypeMay sym (AtPath (Maybe (f a)) (Field Nullable Array sym ': p1 ': p2))
    = Just (Maybe (f (AtPath a (p1 ': p2))))
  AtPathFieldTypeMay sym other = Nothing

type ThrowIfNothing :: Symbol -> Maybe k -> k
type family ThrowIfNothing sym a where
  ThrowIfNothing sym Nothing = TypeError ((Text "Field not found: ") :<>: Text sym)
  ThrowIfNothing sym (Just a) = a

type AtPathFieldType :: Symbol -> Type -> Type
type family AtPathFieldType sym a where
  AtPathFieldType sym a = ThrowIfNothing sym (AtPathFieldTypeMay sym a)

instance
  ( resTy ~ AtPathFieldType sym (AtPath a path)
  , Coercible resTy (AtPath a path)
  ) => HasField sym (AtPath a path) resTy where
  getField = coerce
