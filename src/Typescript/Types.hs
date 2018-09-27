module Typescript.Types where

import Data.Text

data TSType =
    TSPrimitiveType TSPrimitive
  | TSCollectionType TSCollection
  | TSAny
  | TSOption TSType
  | TSInterface Text [TSField]
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
