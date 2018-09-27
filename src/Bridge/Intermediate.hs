module Bridge.Intermediate where

import Data.Text

data BType =
    BPrimitiveType BPrimitive
  | BCollectionType BCollection
  | BOption BType
  | BInterface Text [BField]
   deriving (Eq, Show)


newtype FieldName = FieldName Text deriving (Eq, Show)
data BField = BField FieldName BType deriving (Eq, Show)

data BCollection =
  BArray BType
   deriving (Eq, Show)

data BPrimitive =
    BNumber
  | BString
  | BBoolean
    deriving (Eq, Show)
