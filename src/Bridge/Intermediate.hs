module Bridge.Intermediate where

import Data.Text

data BType =
    BPrimitiveType BPrimitive
  | BCollectionType BCollection
  | BOption BType
  | BConstructed Text BConstructor
   deriving (Eq, Show)

data BConstructor =
    BRecordConstructor [BField]
  | UnionConstructor [BConstructor]
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
