{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
module Main (main) where

import           Data.Generics.Product.Fields
import           GHC.Generics
import           Optics.Core
import           Test.Tasty
import           Test.Tasty.HUnit

import           Payload.Extend.Generics.Optics

main :: IO ()
main = defaultMain $
  testGroup "pay-bump-optics"
    [ testGroup "plain"
      [ testCase "view left only" $
          plain ^. field' @"b1f1" @?= "b1f1"
      , testCase "view right only" $
          plain ^. field' @"e1f1" @?= 1
      , testCase "view both" $
          plain ^. field' @"b1f3" @?= (b2 `ExtendWith` e2)
      , testCase "view nested" $ do
          plain ^. field' @"b1f3" % field' @"b2f2" @?= Just "b2f2"
          plain ^. field' @"b1f3" % field' @"e2f1" @?= False
          plain ^. field' @"b1f3" % field' @"b2f1"
            @?= ([b3_1, b3_2] `ExtendWith` [e3_1, e3_2])
      , testCase "set left only" $
          (field' @"b1f1" .~ "new") plain
            @?= (B1 "new" True b2 `ExtendWith` e1)
      , testCase "set right only" $
          (field' @"e1f1" .~ 5) plain
            @?= (b1 `ExtendWith` E1 5 e2)
      , testCase "set both" $ do
          let newB2 = B2 [] Nothing
              newE2 = E2 [] True
          (field' @"b1f3" .~ (newB2 `ExtendWith` newE2)) plain
            @?= (B1 "b1f1" True newB2 `ExtendWith` E1 1 newE2)
      , testCase "set nested" $ do
          (field' @"b1f3" % field' @"b2f2" .~ Nothing) plain
            @?= (B1 "b1f1" True (B2 [b3_1, b3_2] Nothing) `ExtendWith` e1)
          (field' @"b1f3" % field' @"e2f1" .~ True) plain
            @?= (b1 `ExtendWith` E1 1 (E2 [e3_1, e3_2] True))
      , testCase "over" $
          over (field' @"e1f1") (+ 10) plain
            @?= (b1 `ExtendWith` E1 11 e2)
      , testCase "lens laws" $ do
          (field' @"b1f3" .~ (plain ^. field' @"b1f3")) plain @?= plain
          let v = B2 [] Nothing `ExtendWith` E2 [] True
          (field' @"b1f3" .~ v) plain ^. field' @"b1f3" @?= v
      ]
    , testGroup "atPath"
      [ testCase "view" $ do
          atPath ^. field' @"b1f1" @?= "b1f1"
          atPath ^. field' @"e1f1" @?= 1
          atPath ^. field' @"b1f3" % field' @"b2f2" @?= Just "b2f2"
          atPath ^. field' @"b1f3" % field' @"e2f1" @?= False
          atPath ^. field' @"b1f3" % field' @"b2f1"
            @?= ([b3_1, b3_2] `ExtendWith` [e3_1, e3_2])
      , testCase "view AtPath directly" $ do
          AtPath @_ @'[Req "e1f1"] (1 :: Int) ^. field' @"e1f1" @?= 1
          let nested = AtPath @_ @'[Req "b1f3", Req "e2f1"] False
          nested ^. field' @"b1f3" % field' @"e2f1" @?= False
      , testCase "set" $ do
          (field' @"e1f1" .~ 7) atPath @?= mkAtPath b1 7 False [e3_1, e3_2]
          (field' @"b1f3" % field' @"e2f1" .~ True) atPath
            @?= mkAtPath b1 1 True [e3_1, e3_2]
          (field' @"b1f3" % field' @"b2f2" .~ Nothing) atPath
            @?= mkAtPath (B1 "b1f1" True (B2 [b3_1, b3_2] Nothing)) 1 False [e3_1, e3_2]
      , testCase "set both through array path" $ do
          let newB3s = [B3 9.9]
              newE3s = [E3 Nothing]
          (field' @"b1f3" % field' @"b2f1" .~ (newB3s `ExtendWith` newE3s)) atPath
            @?= mkAtPath (B1 "b1f1" True (B2 newB3s (Just "b2f2"))) 1 False newE3s
      , testCase "set AtPath directly" $
          (field' @"e1f1" .~ 3) (AtPath @_ @'[Req "e1f1"] (1 :: Int))
            @?= AtPath 3
      ]
    ]

type AtPathExtended =
  B1
  `ExtendWith` AtPath Int '[Req "e1f1"]
  `ExtendWith` AtPath Bool '[Req "b1f3", Req "e2f1"]
  `ExtendWith` AtPath [E3] '[Req "b1f3", Arr "b2f1"]

mkAtPath :: B1 -> Int -> Bool -> [E3] -> AtPathExtended
mkAtPath b i e es =
  b `ExtendWith` AtPath i `ExtendWith` AtPath e `ExtendWith` AtPath es

atPath :: AtPathExtended
atPath = mkAtPath b1 1 False [e3_1, e3_2]

plain :: B1 `ExtendWith` E1
plain = b1 `ExtendWith` e1

data B1 = B1
  { b1f1 :: String
  , b1f2 :: Bool
  , b1f3 :: B2
  } deriving stock (Generic, Eq, Show)

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

e1 :: E1
e1 = E1
  { e1f1 = 1
  , b1f3 = e2
  }

data B2 = B2
  { b2f1 :: [B3]
  , b2f2 :: Maybe String
  } deriving stock (Generic, Eq, Show)

b2 :: B2
b2 = B2
  { b2f1 = [b3_1, b3_2]
  , b2f2 = Just "b2f2"
  }

data E2 = E2
  { b2f1 :: [E3]
  , e2f1 :: Bool
  } deriving stock (Generic, Eq, Show)

e2 :: E2
e2 = E2
  { b2f1 = [e3_1, e3_2]
  , e2f1 = False
  }

newtype B3 = B3
  { b3f1 :: Double
  } deriving stock (Generic, Eq, Show)

b3_1 :: B3
b3_1 = B3 { b3f1 = 3.1 }

b3_2 :: B3
b3_2 = B3 { b3f1 = 3.2 }

newtype E3 = E3
  { e3f1 :: Maybe Bool
  } deriving stock (Generic, Eq, Show)

e3_1 :: E3
e3_1 = E3 { e3f1 = Just True }

e3_2 :: E3
e3_2 = E3 { e3f1 = Just False }
