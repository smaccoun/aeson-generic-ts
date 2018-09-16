module Typescript.Types where

import Data.Text

data TSType =
    TSPrimitiveType TSPrimitive
  | TSInterface Text [TSField]
   deriving (Eq, Show)


newtype FieldName = FieldName Text deriving (Eq, Show)
data TSField = TSField IsRequired FieldName TSType deriving (Eq, Show)

data IsRequired = Required | Optional deriving (Eq, Show)

data TSPrimitive =
    TSNumber
  | TSString
  | TSOption TSType
    deriving (Eq, Show)
