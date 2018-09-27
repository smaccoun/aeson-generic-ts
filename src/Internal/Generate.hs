module Internal.Generate where

import           Data.Maybe
import           Data.Monoid      ((<>))
import           Data.Text
import           GenericMappings
import           Typescript.Types

class GenerateTypescript a where
  toTypescript :: a -> Text

instance GenerateTypescript TSType where
  toTypescript (TSPrimitiveType tsPrim) = toTypescript tsPrim
  toTypescript (TSCollectionType tsCollection) = toTypescript tsCollection
  toTypescript (TSOption tsType') = toTypescript tsType' <> " | null "
  toTypescript (TSInterface iName fields') =
      ("interface " <> iName <> " { \n"
      <> toTypescript fields'
      <> "}"
      )
  toTypescript TSAny = "any"


instance GenerateTypescript TSField where
  toTypescript (TSField (FieldName fieldName') fieldType') =
      fieldName' <> " : " <> typeName
      where
        typeName =
          case fieldType' of
              TSPrimitiveType primitive -> toTypescript primitive
              TSInterface iName _       -> iName
              TSCollectionType tCollection       -> toTypescript tCollection
              TSOption tsOption -> toTypescript tsOption
              TSAny -> "any"

instance GenerateTypescript [TSField] where
  toTypescript []     = ""
  toTypescript (x:xs) = "   " <> toTypescript x <> " \n" <> toTypescript xs

instance GenerateTypescript TSPrimitive where
  toTypescript TSNumber           = "number"
  toTypescript TSString           = "string"
  toTypescript TSBoolean          = "boolean"

instance GenerateTypescript TSCollection where
  toTypescript (TSArray tsType') = "Array<" <> toTypescript tsType' <> ">"

data GenMany = forall a . TypescriptType a => GenMany a

genTypescript :: [GenMany] -> Text
genTypescript [] = ""
genTypescript ((GenMany x):xs) =
   (printTS x) <> "\n" <> genTypescript xs

printTS :: (TypescriptType a) => a -> Text
printTS tsType' =
        fromMaybe ""
    $   toTypescript
    <$> toTypescriptType tsType'
