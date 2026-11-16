{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Payload.Extend.Generics
  ( module Extend
  ) where

import           Data.Kind
import           Data.Proxy
import           GHC.Generics
import           GHC.Records
import           GHC.TypeLits

import           Payload.Extend as Extend

--------------------------------------------------------------------------------
-- HasField instance
--------------------------------------------------------------------------------

type ThrowIfNothing :: Maybe k -> k
type family ThrowIfNothing r where
  ThrowIfNothing Nothing = TypeError ('Text "Field Not Found")
  ThrowIfNothing (Just r) = r

type MkGetFieldResult :: Maybe Type -> Maybe Type -> Maybe GetFieldResult
type family MkGetFieldResult l r where
  MkGetFieldResult Nothing Nothing = Nothing
  MkGetFieldResult Nothing (Just r) = Just (RightOnly r)
  MkGetFieldResult (Just l) Nothing = Just (LeftOnly l)
  MkGetFieldResult (Just l) (Just r) = Just (Both l r)

type family GetFieldFromExtended sym l r where
  GetFieldFromExtended sym l r = ThrowIfNothing (GetFieldFromExtendedMay sym l r)

type GetFieldFromExtendedMay :: Symbol -> Type -> Type -> Maybe GetFieldResult
type family GetFieldFromExtendedMay sym l r where
  GetFieldFromExtendedMay sym l r =
    MkGetFieldResult
      (GetFieldType sym l)
      (GetFieldType sym r)

type GetFieldType :: Symbol -> Type -> Maybe Type
type family GetFieldType sym a where
  GetFieldType sym (l `ExtendWith` r) =
    FieldResultToMaybe (GetFieldFromExtendedMay sym l r)
  GetFieldType sym (AtPath a path) = AtPathFieldType sym (AtPath a path)
  GetFieldType sym a = GetFieldFromRep sym (Rep a)

type GetFieldFromRep :: Symbol -> (Type -> Type) -> Maybe Type
type family GetFieldFromRep sym rep where
  GetFieldFromRep sym (D1 lMetaD (C1 lMetaC lFields)) =
    GetFieldFromRepInner sym lFields

type GetFieldFromRepInner :: Symbol -> (Type -> Type) -> Maybe Type
type family GetFieldFromRepInner sym r where
  GetFieldFromRepInner sym (S1 (MetaSel (Just sym) p ss dl) (K1 kr ty)) = Just ty
  GetFieldFromRepInner sym (S1 (MetaSel (Just sym) p ss dl) (K1 kr ty) :*: rest) = Just ty
  GetFieldFromRepInner sym (S1 (MetaSel sel p ss dl) (K1 kr ty) :*: rest) = GetFieldFromRepInner sym rest
  GetFieldFromRepInner sym r = Nothing

type data GetFieldResult
  = LeftOnly Type
  | RightOnly Type
  | Both Type Type

type FieldResultToType :: GetFieldResult -> Type
type family FieldResultToType r where
  FieldResultToType (LeftOnly t) = t
  FieldResultToType (RightOnly t) = t
  FieldResultToType (Both l r) = l `ExtendWith` r

type FieldResultToMaybe :: Maybe GetFieldResult -> Maybe Type
type family FieldResultToMaybe r where
  FieldResultToMaybe (Just fr) = Just (FieldResultToType fr)
  FieldResultToMaybe Nothing = Nothing

type GetField :: Symbol -> GetFieldResult -> Type -> Constraint
class GetField sym fr a where
  getFieldAux :: Proxy sym -> Proxy fr -> a -> FieldResultToType fr

instance HasField sym l resTy
    => GetField sym (LeftOnly resTy) (l `ExtendWith` r) where
  getFieldAux _ _ (l `ExtendWith` _) = getField @sym l

instance HasField sym r resTy
    => GetField sym (RightOnly resTy) (l `ExtendWith` r) where
  getFieldAux _ _ (_ `ExtendWith` r) = getField @sym r

instance (HasField sym l lTy, HasField sym r rTy)
    => GetField sym (Both lTy rTy) (l `ExtendWith` r) where
  getFieldAux _ _ (l `ExtendWith` r) = getField @sym l `ExtendWith` getField @sym r

instance
  ( RequireGeneric l
  , RequireGeneric r
  , fr ~ GetFieldFromExtended sym l r
  , resTy ~ FieldResultToType fr
  , GetField sym fr (l `ExtendWith` r)
  ) => HasField sym (l `ExtendWith` r) resTy
  where
    getField = getFieldAux (Proxy @sym) (Proxy @fr)

-- | Used to keep a stuck type family from surfacing if a Generic instance is missing
class RequireGeneric a
instance {-# OVERLAPPING #-} (RequireGeneric l, RequireGeneric r) => RequireGeneric (l `ExtendWith` r)
instance {-# OVERLAPPING #-} RequireGeneric (AtPath a path)
instance Generic a => RequireGeneric a
