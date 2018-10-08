module Typescript.Types
  (module Typescript.Types
   ,module Typescript.Primitives
   ) where

import           Data.Text
import           Typescript.Primitives

data TSType =
    TSPrimitiveType TSPrimitive
  | TSCollectionType TSCollection
  | TSAny
  | TSCustomType TSCustom
  | TSInterface Text [TSField]
  | TSUnion Text [TSType]
   deriving (Eq, Show)

data TSCustom =
  TSOption TSType deriving (Eq, Show)

newtype FieldName = FieldName Text deriving (Eq, Show)
data TSField = TSField FieldName TSType deriving (Eq, Show)

data TSCollection =
  TSArray TSType
   deriving (Eq, Show)

