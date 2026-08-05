{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Data.Function
#if MIN_VERSION_openapi3(3,2,5)
import qualified Data.HashMap.Strict.InsOrd.Compat as IOHM
#else
import qualified Data.HashMap.Strict.InsOrd as IOHM
#endif
import qualified Data.List as List
import           Data.OpenApi
import           Data.Proxy
import           GHC.Generics hiding (C1)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Payload.Extend

data CO1 = CO1
  { b1f1 :: String
  , b1f2 :: Bool
  , b1f3 :: CO2
  , e1f1 :: Maybe Int
  } deriving stock (Generic, Eq, Show)
    deriving anyclass ToSchema

data CO2 = CO2
  { b2f1 :: [CO3]
  , b2f2 :: Maybe String
  , b2f3 :: CO4
  , e2f1 :: Maybe Bool
  } deriving stock (Generic, Eq, Show)
    deriving anyclass ToSchema

data CO3 = CO3
  { b3f1 :: Double
  , e3f1 :: Maybe Bool
  } deriving stock (Generic, Eq, Show)
    deriving anyclass ToSchema

data CO4 = CO4
  { b4f1 :: String
  , e4f1 :: Maybe String
  , e4f2 :: Maybe CO5
  } deriving stock (Generic, Eq, Show)
    deriving anyclass ToSchema

newtype CO5 = CO5
  { e5f1 :: String
  } deriving stock (Generic, Eq, Show)
    deriving anyclass ToSchema

$(deriveJSON defaultOptions { omitNothingFields = True } 'CO5)
$(deriveJSON defaultOptions { omitNothingFields = True } 'CO4)
$(deriveJSON defaultOptions { omitNothingFields = True } 'CO3)
$(deriveJSON defaultOptions { omitNothingFields = True } 'CO2)
$(deriveJSON defaultOptions { omitNothingFields = True } 'CO1)

main :: IO ()
main = defaultMain $
  testGroup "pay-bump"
    [ testCase "ToJSON" $
        toJSON (b1 `ExtendWith` e1)
          @?= toJSON c1
    , testCase "FromJSON" $
        parseEither (parseJSON @(B1 `ExtendWith` E1)) (toJSON (b1 `ExtendWith` e1))
          @?= Right (b1 `ExtendWith` e1)
    , testCase "ToSchema" $
        eqSchema
          (toInlinedSchema (Proxy @(B1 `ExtendWith` E1)))
          (toInlinedSchema (Proxy @C1))
    , testCase "ToJSON OptionalObj Nothing" $
        toJSON (b1 `ExtendWith` OptionalObj @E1 Nothing)
          @?= toJSON b1
    , testCase "ToJSON OptionalObj Just" $
        toJSON (b1 `ExtendWith` OptionalObj (Just e1))
          @?= toJSON co1
    -- , testCase "FromJSON OptionalObj Nothing" $
    --     fmap toJSON (parseEither (parseJSON @(B1 `ExtendWith` OptionalObj E1)) (toJSON (b1 `ExtendWith` OptionalObj @E1 Nothing)))
    --       @?= fmap toJSON (parseEither (parseJSON @CO1) (toJSON (b1 `ExtendWith` OptionalObj @E1 Nothing)))
    -- , testCase "FromJSON OptionalObj Just" $
    --     parseEither (parseJSON @(B1 `ExtendWith` OptionalObj E1)) (toJSON (b1 `ExtendWith` OptionalObj (Just e1)))
    --       @?= Right (b1 `ExtendWith` OptionalObj (Just e1))
    , testCase "ToSchema OptionalObj" $
        eqSchema
          (toInlinedSchema (Proxy @(B1 `ExtendWith` OptionalObj E1)))
          (toInlinedSchema (Proxy @CO1))
    ]

data B1 = B1
  { b1f1 :: String
  , b1f2 :: Bool
  , b1f3 :: B2
  } deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

b1 :: B1
b1 = B1
  { b1f1 = "b1f1"
  , b1f2 = True
  , b1f3 = b2
  }

data E1 = E1
  { e1f1 :: Int
  , b1f3 :: E2
  } deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

e1 :: E1
e1 = E1
  { e1f1 = 1
  , b1f3 = e2
  }

data C1 = C1
  { b1f1 :: String
  , b1f2 :: Bool
  , b1f3 :: C2
  , e1f1 :: Int
  } deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

c1 :: C1
c1 = C1
  { b1f1 = b1.b1f1
  , b1f2 = b1.b1f2
  , b1f3 = c2
  , e1f1 = e1.e1f1
  }

co1 :: CO1
co1 = CO1
  { b1f1 = b1.b1f1
  , b1f2 = b1.b1f2
  , b1f3 = co2
  , e1f1 = Just e1.e1f1
  }

data B2 = B2
  { b2f1 :: [B3]
  , b2f2 :: Maybe String
  , b2f3 :: B4
  } deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

b2 :: B2
b2 = B2
  { b2f1 = [b3_1, b3_2]
  , b2f2 = Just "b2f2"
  , b2f3 = b4
  }

data E2 = E2
  { b2f1 :: [E3]
  , b2f3 :: Maybe E4
  , e2f1 :: Bool
  } deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

e2 :: E2
e2 = E2
  { b2f1 = [e3_1, e3_2]
  , b2f3 = Just e4
  , e2f1 = False
  }

data C2 = C2
  { b2f1 :: [C3]
  , b2f2 :: Maybe String
  , b2f3 :: C4
  , e2f1 :: Bool
  } deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

c2 :: C2
c2 = C2
  { b2f1 = [c3_1, c3_2]
  , b2f2 = b2.b2f2
  , b2f3 = c4
  , e2f1 = e2.e2f1
  }

co2 :: CO2
co2 = CO2
  { b2f1 = [co3_1, co3_2]
  , b2f2 = b2.b2f2
  , b2f3 = co4
  , e2f1 = Just e2.e2f1
  }

newtype B3 = B3
  { b3f1 :: Double
  } deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

b3_1 :: B3
b3_1 = B3
  { b3f1 = 3.1
  }

b3_2 :: B3
b3_2 = B3
  { b3f1 = 3.2
  }

newtype E3 = E3
  { e3f1 :: Maybe Bool
  } deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

e3_1 :: E3
e3_1 = E3
  { e3f1 = Just True
  }

e3_2 :: E3
e3_2 = E3
  { e3f1 = Just False
  }

data C3 = C3
  { b3f1 :: Double
  , e3f1 :: Maybe Bool
  } deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

c3_1 :: C3
c3_1 = C3
  { b3f1 = b3_1.b3f1
  , e3f1 = e3_1.e3f1
  }

c3_2 :: C3
c3_2 = C3
  { b3f1 = b3_2.b3f1
  , e3f1 = e3_2.e3f1
  }

co3_1 :: CO3
co3_1 = CO3
  { b3f1 = b3_1.b3f1
  , e3f1 = e3_1.e3f1
  }

co3_2 :: CO3
co3_2 = CO3
  { b3f1 = b3_2.b3f1
  , e3f1 = e3_2.e3f1
  }

newtype B4 = B4
  { b4f1 :: String
  } deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

b4 :: B4
b4 = B4 "b4f1"

data E4 = E4
  { e4f1 :: String
  , e4f2 :: E5
  } deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

e4 :: E4
e4 = E4
  { e4f1 = "e4f1"
  , e4f2 = e5
  }

data C4 = C4
  { b4f1 :: String
  , e4f1 :: Maybe String
  , e4f2 :: Maybe E5
  } deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

c4 :: C4
c4 = C4
  { b4f1 = b4.b4f1
  , e4f1 = Just e4.e4f1
  , e4f2 = Just e5
  }

co4 :: CO4
co4 = CO4
  { b4f1 = b4.b4f1
  , e4f1 = Just e4.e4f1
  , e4f2 = Just co5
  }

co5 :: CO5
co5 = CO5 "e5f1"

newtype E5 = E5
  { e5f1 :: String
  } deriving stock (Generic, Eq, Show)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

e5 :: E5
e5 = E5 "e5f1"

eqSchema :: Schema -> Schema -> Assertion
eqSchema a b
  = do
  a { _schemaProperties = mempty, _schemaRequired = mempty }
    @?= b { _schemaProperties = mempty, _schemaRequired = mempty }
  void propsMatch
  List.sort (_schemaRequired a) @?= List.sort (_schemaRequired b)
  where
    propsMatch = do
      let pa = List.sortOn fst (IOHM.toList (_schemaProperties a))
          pb = List.sortOn fst (IOHM.toList (_schemaProperties b))
      ((@?=) `on` fmap fst) pa pb
      zipWithM
        (eqSchema `on` deRef . snd)
        (List.sortOn fst (IOHM.toList (_schemaProperties a)))
        (List.sortOn fst (IOHM.toList (_schemaProperties b)))
    deRef (Inline s) = s
    deRef (Ref _) = error "impossible"
