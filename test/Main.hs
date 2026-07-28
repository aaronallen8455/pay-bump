{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main (main) where

import           Control.Monad
import           Data.Aeson
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

import           Data.Extend

import           Debug.Trace

main :: IO ()
main = defaultMain $
  testGroup "pay-bump"
    [ testCase "ToJSON" $
        toJSON (b1 `ExtendedWith` e1)
          @?= toJSON c1
    , testCase "ToSchema" $
        eqSchema
          (toInlinedSchema (Proxy @(B1 `ExtendedWith` E1)))
          (toInlinedSchema (Proxy @C1))
    ]

data B1 = B1
  { b1f1 :: String
  , b1f2 :: Bool
  , b1f3 :: B2
  } deriving stock Generic
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
  } deriving stock Generic
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
  } deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)

c1 :: C1
c1 = C1
  { b1f1 = b1.b1f1
  , b1f2 = b1.b1f2
  , b1f3 = c2
  , e1f1 = e1.e1f1
  }

data B2 = B2
  { b2f1 :: [B3]
  , b2f2 :: Maybe String
  } deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)

b2 :: B2
b2 = B2
  { b2f1 = [b3_1, b3_2]
  , b2f2 = Just "b2f2"
  }

data E2 = E2
  { b2f1 :: [E3]
  , e2f1 :: Bool
  } deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)

e2 :: E2
e2 = E2
  { b2f1 = [e3_1, e3_2]
  , e2f1 = False
  }

data C2 = C2
  { b2f1 :: [C3]
  , b2f2 :: Maybe String
  , e2f1 :: Bool
  } deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)

c2 :: C2
c2 = C2
  { b2f1 = [c3_1, c3_2]
  , b2f2 = b2.b2f2
  , e2f1 = e2.e2f1
  }

newtype B3 = B3
  { b3f1 :: ()
  } deriving stock Generic
    deriving anyclass (ToJSON, FromJSON, ToSchema)

b3_1 :: B3
b3_1 = B3
  { b3f1 = ()
  }

b3_2 :: B3
b3_2 = B3
  { b3f1 = ()
  }

newtype E3 = E3
  { e3f1 :: Maybe Bool
  } deriving stock Generic
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
  { b3f1 :: ()
  , e3f1 :: Maybe Bool
  } deriving stock Generic
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

eqSchema :: Schema -> Schema -> Assertion
eqSchema a b
  = do
  a { _schemaProperties = mempty, _schemaRequired = mempty }
    @?= b { _schemaProperties = mempty, _schemaRequired = mempty }
  propsMatch
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
