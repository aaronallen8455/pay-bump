{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
module Payload.NameSchema
  ( NameSchema(..)
  ) where

import           Control.DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.OpenApi as OpenApi
import           Data.Proxy
import qualified Data.Text as T
import           GHC.TypeLits

-- | Allows for specifying a name to be used for the OpenApi schema declaration.
newtype NameSchema (name :: Symbol) a = NameSchema a
  deriving (Show, Read, Eq, Ord, Functor)
  deriving newtype (Aeson.ToJSON, Aeson.FromJSON, NFData)

instance (OpenApi.ToSchema a, KnownSymbol name) => OpenApi.ToSchema (NameSchema name a) where
  declareNamedSchema _ =
    (\ns -> ns { OpenApi._namedSchemaName = Just . T.pack $ symbolVal (Proxy @name) })
      <$> OpenApi.declareNamedSchema (Proxy @a)
