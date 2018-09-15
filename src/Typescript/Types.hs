module Typescript.Types where

import Data.Text

data TSType =
    TSPrimitiveType TSPrimitive
  | TSInterface Text [TSField]
   deriving (Show)


newtype FieldName = FieldName Text deriving (Show)
data TSField = TSField FieldName TSType deriving (Show)

data TSPrimitive =
    TSNumber
  | TSString deriving (Show)
