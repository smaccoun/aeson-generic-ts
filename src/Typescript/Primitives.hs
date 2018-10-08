module Typescript.Primitives where

import           Data.Text (Text)

data TSPrimitive =
    TSNumber
  | TSString
  | TSBoolean
    deriving (Eq, Show)


generateTSPrimitive :: TSPrimitive -> Text
generateTSPrimitive prim =
  case prim of
    TSNumber  -> "number"
    TSString  -> "string"
    TSBoolean -> "boolean"



