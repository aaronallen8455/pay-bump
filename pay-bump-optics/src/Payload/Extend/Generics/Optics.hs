{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Payload.Extend.Generics.Optics
  ( module Generics
  , baseLens
  , extLens
  ) where

import           Data.Coerce
import           Data.Generics.Product.Fields
import           Data.Proxy
import           Optics.Core
import           Payload.Extend.Generics as Generics

instance {-# OVERLAPPING #-}
  ( res ~ AtPathFieldType sym (AtPath a path)
  , Coercible a res
  ) => HasField' sym (AtPath a path) res where
  field' = lens coerce (\_ res -> coerce res)

instance {-# OVERLAPPING #-}
  ( RequireGeneric b
  , RequireGeneric ext
  , gfRes ~ GetFieldFromExtended sym b ext
  , resTy ~ FieldResultToType gfRes
  , HasFieldAux sym gfRes (ExtendWith b ext)
  ) => HasField' sym (b `ExtendWith` ext) resTy where
  field' = fieldAux (Proxy @sym) (Proxy @gfRes)

class HasFieldAux sym fieldRes a where
  fieldAux
    :: Proxy sym
    -> Proxy fieldRes
    -> Lens' a (FieldResultToType fieldRes)

instance HasField' sym l resTy
    => HasFieldAux sym (LeftOnly resTy) (l `ExtendWith` r) where
  fieldAux _ _ = baseLens % field' @sym

instance HasField' sym r resTy
    => HasFieldAux sym (RightOnly resTy) (l `ExtendWith` r) where
  fieldAux _ _ = extLens % field' @sym

instance
  ( HasField' sym l lRes
  , HasField' sym r rRes
  ) => HasFieldAux sym (Both lRes rRes) (l `ExtendWith` r) where
  fieldAux _ _ = lens getter setter
    where
      getter (l `ExtendWith` r) = (l ^. field' @sym) `ExtendWith` (r ^. field' @sym)
      setter (l `ExtendWith` r) (lr `ExtendWith` rr) =
        (field' @sym .~ lr) l `ExtendWith` (field' @sym .~ rr) r

baseLens :: Lens (l `ExtendWith` r) (l' `ExtendWith` r) l l'
baseLens = lens (\(l `ExtendWith` _) -> l) (\(_ `ExtendWith` r) l -> l `ExtendWith` r)

extLens :: Lens (l `ExtendWith` r) (l `ExtendWith` r') r r'
extLens = lens (\(_ `ExtendWith` r) -> r) (\(l `ExtendWith` _) r -> l `ExtendWith` r)
