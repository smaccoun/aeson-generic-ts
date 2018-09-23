module Internal.Types where

import Data.Text

data TSType =
    TSPrimitiveType TSPrimitive
  | TSInterface Text [TSField]
  | TSCollectionType TSCollection
  | TSOption TSType
  | TSAny
   deriving (Eq, Show)


newtype FieldName = FieldName Text deriving (Eq, Show)
data TSField = TSField FieldName TSType deriving (Eq, Show)

data TSCollection =
  TSArray TSType
   deriving (Eq, Show)

data TSPrimitive =
    TSNumber
  | TSString
  | TSBoolean
    deriving (Eq, Show)
