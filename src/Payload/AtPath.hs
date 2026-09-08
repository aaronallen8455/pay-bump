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
  ) where

import           Control.DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import           Data.Coerce
#if MIN_VERSION_openapi3(3,2,5)
import qualified Data.HashMap.Strict.InsOrd.Compat as IOHM
#else
import qualified Data.HashMap.Strict.InsOrd as IOHM
#endif
import qualified Data.Generics as Syb
import qualified Data.List as List
import qualified Data.OpenApi as OpenApi
import           Data.Proxy
import qualified Data.Text as T
import           GHC.TypeLits

type Path = [PathField]

type data Optionality
  = Optional
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

-- Dispatch wrappers used to explicitly guide instance resolution
newtype OptionalField a = OptionalField a
newtype NullableField a = NullableField a
newtype RegularField a = RegularField a
newtype ArrayField a = ArrayField a

-- Guarding the coercible constraint behind the Wrap and Unwrap classes allows
-- for keeping the dispatch wrappers internal.
class Wrap r where
  wrap :: a -> r a
  default wrap :: Coercible a (r a) => a -> r a
  wrap = coerce
instance Wrap OptionalField
instance Wrap NullableField
instance Wrap RegularField
instance Wrap ArrayField

class Unwrap r where
  unwrap :: r a -> a
  default unwrap :: Coercible a (r a) => r a -> a
  unwrap = coerce
instance Unwrap OptionalField
instance Unwrap NullableField
instance Unwrap RegularField
instance Unwrap ArrayField

type family Dispatch atPath where
  Dispatch (AtPath (Maybe a) (Field Optional c sym ': p))
    = OptionalField (AtPath (Maybe a) (Field Optional c sym ': p))
  Dispatch (AtPath (Maybe a) fp)
    = NullableField (AtPath (Maybe a) fp)
  Dispatch (AtPath (f a) (Field Required Array sym ': p))
    = ArrayField (AtPath (f a) (Field Required Array sym ': p))
  Dispatch (AtPath a (Field Required NonArray sym ': p))
    = RegularField (AtPath a (Field Required NonArray sym ': p))
  Dispatch (AtPath a (Field opt c sym ': p))
    = TypeError (Text "Invalid JSON path component at key '" :<>: Text sym :<>: Text "'")

--------------------------------------------------------------------------------
-- ToJSON
--------------------------------------------------------------------------------

instance Aeson.ToJSON a => Aeson.ToJSON (AtPath a '[]) where
  toJSON = Aeson.toJSON @a . coerce

instance
  ( d (AtPath a (p ': ps)) ~ Dispatch (AtPath a (p ': ps))
  , Wrap d
  , Aeson.ToJSON (d (AtPath a (p ': ps)))
  )
  => Aeson.ToJSON (AtPath a (p ': ps)) where
  toJSON = Aeson.toJSON . wrap @d

instance Aeson.ToJSON (AtPath a (Field Required c sym ': p))
  => Aeson.ToJSON (OptionalField (AtPath (Maybe a) (Field Optional c sym ': p))) where
  toJSON
    = maybe
        (Aeson.object [])
        (Aeson.toJSON @(AtPath a (Field Required c sym ': p)))
    . coerce

instance
  ( Aeson.ToJSON (AtPath a (Field Required c sym ': p))
  , KnownSymbol sym
  ) => Aeson.ToJSON (NullableField (AtPath (Maybe a) (Field Required c sym ': p))) where
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
  ) => Aeson.ToJSON (ArrayField (AtPath (f a) (Field Required Array sym ': p))) where
  toJSON x =
    Aeson.object
      [ Key.fromString (symbolVal (Proxy @sym))
          Aeson..= fmap Aeson.toJSON (coerce @_ @(f (AtPath a p)) x)
      ]

instance
  ( Aeson.ToJSON (AtPath a p)
  , KnownSymbol sym
  ) => Aeson.ToJSON (RegularField (AtPath a (Field Required NonArray sym ': p))) where
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
  ( d (AtPath a (p ': ps)) ~ Dispatch (AtPath a (p ': ps))
  , Unwrap d
  , Aeson.FromJSON (d (AtPath a (p ': ps)))
  )
  => Aeson.FromJSON (AtPath a (p ': ps)) where
  parseJSON = fmap (unwrap @d) . Aeson.parseJSON

instance
  ( Aeson.FromJSON (AtPath a (Field Required c sym ': p))
  , KnownSymbol sym
  ) => Aeson.FromJSON (OptionalField (AtPath (Maybe a) (Field Optional c sym ': p))) where
  parseJSON
    = Aeson.withObject "AtPath" $ \obj ->
      case KeyMap.lookup (Key.fromString (symbolVal (Proxy @sym))) obj of
        Nothing -> pure $ coerce (Nothing @a)
        Just Aeson.Null -> pure $ coerce (Nothing @a)
        Just _ -> coerce . Just
          <$> Aeson.parseJSON @(AtPath a (Field Required c sym ': p)) (Aeson.Object obj)

-- Handle nullability
instance
  ( Aeson.FromJSON (AtPath a (Field Required c sym ': p))
  , KnownSymbol sym
  ) => Aeson.FromJSON (NullableField (AtPath (Maybe a) (Field Required c sym ': p))) where
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
  ) => Aeson.FromJSON (ArrayField (AtPath (f a) (Field Required Array sym ': p))) where
  parseJSON = Aeson.withObject "AtPath" $ \obj ->
    coerce $
      Aeson.parseField @(f (AtPath a p))
        obj
        (Key.fromString (symbolVal (Proxy @sym)))

instance
  (Aeson.FromJSON (AtPath a p), KnownSymbol sym)
  => Aeson.FromJSON (RegularField (AtPath a (Field Required NonArray sym ': p))) where
  parseJSON = Aeson.withObject "AtPath" $ \obj ->
    coerce @(Aeson.Parser (AtPath a p))
      $ obj Aeson..: Key.fromString (symbolVal (Proxy @sym))

--------------------------------------------------------------------------------
-- ToSchema
--------------------------------------------------------------------------------

instance OpenApi.ToSchema a => OpenApi.ToSchema (AtPath a '[]) where
  declareNamedSchema _ = OpenApi.declareNamedSchema (Proxy @a)

instance
  ( d ~ Dispatch (AtPath a (p ': ps))
  , Syb.Typeable a , Syb.Typeable ps , Syb.Typeable p
  , OpenApi.ToSchema d
  )
  => OpenApi.ToSchema (AtPath a (p ': ps)) where
  declareNamedSchema _ = OpenApi.declareNamedSchema (Proxy @d)

instance
  ( KnownSymbol sym
  , Syb.Typeable a, Syb.Typeable p, Syb.Typeable c
  , OpenApi.ToSchema (AtPath a (Field Required c sym ': p))
  ) => OpenApi.ToSchema (OptionalField (AtPath (Maybe a) (Field Optional c sym ': p))) where
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
  => OpenApi.ToSchema (NullableField (AtPath (Maybe a) (Field Required c sym ': p))) where
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
  ) => OpenApi.ToSchema (ArrayField (AtPath [a] (Field Required Array sym ': p))) where
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
  ) => OpenApi.ToSchema (RegularField (AtPath a (Field Required NonArray sym ': p))) where
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
