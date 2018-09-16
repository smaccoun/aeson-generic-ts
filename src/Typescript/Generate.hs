module Typescript.Generate where

import Typescript.Types
import Data.Text
import Data.Monoid ((<>))

class GenerateTypescript a where
  toTypescript :: a -> Text

instance GenerateTypescript TSType where
  toTypescript (TSPrimitiveType tsPrim) = toTypescript tsPrim
  toTypescript (TSInterface iName fields') =
    "interface " <> iName <> " { \n"
      <> toTypescript fields'
      <> "}"

instance GenerateTypescript TSField where
  toTypescript (TSField (FieldName fieldName') fieldType') =
    fieldName' <> " : " <> toTypescript fieldType'

instance GenerateTypescript [TSField] where
  toTypescript [] = ""
  toTypescript (x:xs) = "   " <> toTypescript x <> " \n" <> toTypescript xs

instance GenerateTypescript TSPrimitive where
  toTypescript TSNumber = "number"
  toTypescript TSString = "string"
  toTypescript (TSOption tsType') = toTypescript tsType' <> " | null "


genTypescript :: (GenerateTypescript a) => [a] -> Text
genTypescript [] = ""
genTypescript (x:xs) =
   (toTypescript x) <> "\n" <> genTypescript xs
