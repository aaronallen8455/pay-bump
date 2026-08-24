{-# LANGUAGE TypeData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Payload.AtPath
  ( AtPath(..)
  , Path
  , PathSegment(..)
  ) where

import           Control.DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as Aeson
import           Data.Coerce
#if MIN_VERSION_openapi3(3,2,5)
import qualified Data.HashMap.Strict.InsOrd.Compat as IOHM
#else
import qualified Data.HashMap.Strict.InsOrd as IOHM
#endif
import qualified Data.Generics as Syb
import qualified Data.OpenApi as OpenApi
import           Data.Proxy
import qualified Data.Text as T
import           GHC.TypeLits

type Path = [PathSegment]

type data PathSegment
  = Req Symbol -- ^ Denotes a required field
  | Opt Symbol -- ^ Denotes an optional field
  | Arr Symbol -- ^ Denotes an array field

-- | Allows an extension object to be merged at a specific path in the base
-- object. This offers a surgical approach to payload extension where deeply
-- nested extension records can be defined independent of the structure of the
-- base type.
newtype AtPath (path :: Path) a = AtPath a
  deriving (Show, Read, Eq, Ord, Functor)
  deriving newtype (NFData)

instance (Aeson.ToJSON (AtPath p a), KnownSymbol sym)
  => Aeson.ToJSON (AtPath (Req sym ': p) a) where
  toJSON x =
    Aeson.object
      [ Key.fromString (symbolVal (Proxy @sym))
          Aeson..= Aeson.toJSON @(AtPath p a) (coerce x)
      ]

instance
  ( Aeson.ToJSON (AtPath p a)
  , KnownSymbol sym
  , Functor f
  , Aeson.ToJSON (f Aeson.Value)
  , Coercible (f a) (f (AtPath p a))
  ) => Aeson.ToJSON (AtPath (Arr sym ': p) (f a)) where
  toJSON x =
    Aeson.object
      [ Key.fromString (symbolVal (Proxy @sym))
          Aeson..= fmap (Aeson.toJSON @(AtPath p a)) (coerce @_ @(f (AtPath p a)) x)
      ]

instance (Aeson.ToJSON (AtPath p a), KnownSymbol sym)
  => Aeson.ToJSON (AtPath (Opt sym ': p) (Maybe a)) where
  toJSON x =
    Aeson.Object
      $ Key.fromString (symbolVal (Proxy @sym))
          Aeson..?=
            fmap
              (Aeson.toJSON @(AtPath p a))
              (coerce @_ @(Maybe (AtPath p a)) x)

instance Aeson.ToJSON a => Aeson.ToJSON (AtPath '[] a) where
  toJSON = Aeson.toJSON @a . coerce

instance (Aeson.FromJSON (AtPath p a), KnownSymbol sym)
  => Aeson.FromJSON (AtPath (Req sym ': p) a) where
  parseJSON = Aeson.withObject "AtPath" $ \obj ->
    coerce @(Aeson.Parser (AtPath p a))
      $ obj Aeson..: Key.fromString (symbolVal (Proxy @sym))

instance
  ( Aeson.FromJSON (AtPath p a)
  , KnownSymbol sym
  , Aeson.FromJSON (f (AtPath p a))
  , Coercible (f a) (f (AtPath p a))
  ) => Aeson.FromJSON (AtPath (Arr sym ': p) (f a)) where
  parseJSON = Aeson.withObject "AtPath" $ \obj ->
    coerce $
      Aeson.parseField @(f (AtPath p a))
        obj
        (Key.fromString (symbolVal (Proxy @sym)))

instance (Aeson.FromJSON (AtPath p a), KnownSymbol sym)
  => Aeson.FromJSON (AtPath (Opt sym ': p) (Maybe a)) where
  parseJSON = Aeson.withObject "AtPath" $ \obj ->
    coerce $
      Aeson.parseFieldMaybe
        @(AtPath p a)
        obj
        (Key.fromString (symbolVal (Proxy @sym)))

instance Aeson.FromJSON a => Aeson.FromJSON (AtPath '[] a) where
  parseJSON = coerce . Aeson.parseJSON @a

instance
  ( OpenApi.ToSchema (AtPath p a)
  , KnownSymbol sym
  , Syb.Typeable a
  , Syb.Typeable p
  ) => OpenApi.ToSchema (AtPath (Req sym ': p) a) where
  declareNamedSchema _ = do
    ns <- OpenApi.declareNamedSchema (Proxy @(AtPath p a))
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
        , OpenApi._schemaTitle = do
            title <- OpenApi._schemaTitle $ OpenApi._namedSchemaSchema ns
            Just $ key <> "." <> title
        , OpenApi._schemaType = Just OpenApi.OpenApiObject
        }
      }

instance
  ( OpenApi.ToSchema (AtPath p a)
  , KnownSymbol sym
  , Syb.Typeable a
  , Syb.Typeable p
  ) => OpenApi.ToSchema (AtPath (Opt sym ': p) (Maybe a)) where
  declareNamedSchema _ = do
    ns <- OpenApi.declareNamedSchema (Proxy @(AtPath p a))
    let key = T.pack $ symbolVal (Proxy @sym)
        mName = do
          n <- OpenApi._namedSchemaName ns
          Just $ key <> ".?" <> n
    pure OpenApi.NamedSchema
      { OpenApi._namedSchemaName = mName
      , OpenApi._namedSchemaSchema = mempty
        { OpenApi._schemaProperties =
            IOHM.singleton key (OpenApi.Inline $ OpenApi._namedSchemaSchema ns)
        , OpenApi._schemaRequired = []
        , OpenApi._schemaTitle = do
            title <- OpenApi._schemaTitle $ OpenApi._namedSchemaSchema ns
            Just $ key <> ".?" <> title
        , OpenApi._schemaType = Just OpenApi.OpenApiObject
        }
      }

instance
  ( OpenApi.ToSchema (AtPath p a)
  , KnownSymbol sym
  , Syb.Typeable a
  , Syb.Typeable p
  ) => OpenApi.ToSchema (AtPath (Arr sym ': p) [a]) where
  declareNamedSchema _ = do
    ns <- OpenApi.declareNamedSchema (Proxy @(AtPath p a))
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
        , OpenApi._schemaTitle = do
            title <- OpenApi._schemaTitle $ OpenApi._namedSchemaSchema ns
            Just $ key <> "[]." <> title
        , OpenApi._schemaType = Just OpenApi.OpenApiObject
        }
      }

instance OpenApi.ToSchema a => OpenApi.ToSchema (AtPath '[] a) where
  declareNamedSchema _ = OpenApi.declareNamedSchema (Proxy @a)
