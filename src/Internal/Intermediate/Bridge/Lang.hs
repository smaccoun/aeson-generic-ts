{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Internal.Intermediate.Bridge.Lang where

import Data.Text
import Internal.Output.Foreign

class (IsForeignType toLangType) => FromBridge toLangType where
  toForeign :: BType -> Maybe toLangType

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
