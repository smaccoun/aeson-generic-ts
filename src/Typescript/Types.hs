module Typescript.Types where

import Data.Text

data TSType =
    TSPrimitiveType TSPrimitive
  | TSInterface Text [TSField]
  | TSCollectionType TSCollection
   deriving (Eq, Show)


newtype FieldName = FieldName Text deriving (Eq, Show)
data TSField = TSField FieldName TSType deriving (Eq, Show)

data TSCollection =
  TSArray TSType
   deriving (Eq, Show)

data TSPrimitive =
    TSNumber
  | TSString
  | TSOption TSType
    deriving (Eq, Show)
