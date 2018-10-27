{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Internal.Intermediate.Bridge.Lang where

import Data.Text
import GHC.Generics

class (IsForeignType toLangType) => FromBridge toLangType where
  toForeign :: BType -> Maybe toLangType

class IsForeignType t where
  toForeignType :: t -> (ForeignType t)

data ForeignType t =
  ForeignType
    {refName     :: Text
    ,declaration :: Text
    } deriving (Generic, Functor)

selfRefForeign :: Text -> ForeignType t
selfRefForeign ref =
  ForeignType ref ref

data BType =
    BPrimitiveType BPrimitive
  | BCollectionType BCollection
  | BOption BType
  | BConstructed Text BConstructor
   deriving (Eq, Show)

data BConstructor =
    SingleConstructorType [BSingleConstructorArg]
  | UnionConstructor [BConstructor]
    deriving (Eq, Show)

data BSingleConstructorArg =
    OfRecord BField
  | OfUnTagged BType
    deriving (Eq, Show)

newtype BFieldName = BFieldName Text deriving (Eq, Show)
data BField = BField BFieldName BType deriving (Eq, Show)

data BCollection =
  BArray BType
   deriving (Eq, Show)

data BPrimitive =
    BInt
  | BString
  | BBoolean
    deriving (Eq, Show)
