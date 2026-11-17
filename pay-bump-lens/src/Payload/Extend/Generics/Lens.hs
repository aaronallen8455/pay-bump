{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module Payload.Extend.Generics.Lens
  (
  ) where

import           Data.Coerce
import           Data.Generics.Internal.VL
import           Data.Generics.Product.Fields
import           Data.Proxy
import           Payload.Extend.Generics

instance {-# OVERLAPPING #-}
  ( res ~ AtPathFieldType sym (AtPath a path)
  , Coercible a res
  ) => HasField' sym (AtPath a path) res where
  field' f atPath = fmap coerce (f (coerce atPath))

instance {-# OVERLAPPING #-}
  ( gfRes ~ GetFieldFromExtended sym b ext
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
  fieldAux _ _ f (l `ExtendWith` r) = (`ExtendWith` r) <$> field' @sym @l f l

instance HasField' sym r resTy
    => HasFieldAux sym (RightOnly resTy) (l `ExtendWith` r) where
  fieldAux _ _ f (l `ExtendWith` r) = (l `ExtendWith`) <$> field' @sym @r f r

instance
  ( HasField' sym l lRes
  , HasField' sym r rRes
  ) => HasFieldAux sym (Both lRes rRes) (l `ExtendWith` r) where
  fieldAux _ _ f (l `ExtendWith` r)
    = fmap
        (\(lr `ExtendWith` rr) ->
          (field' @sym .~ lr) l `ExtendWith` (field' @sym .~ rr) r
        )
      . f
      $ (l ^. field' @sym) `ExtendWith` (r ^. field' @sym)
