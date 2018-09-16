module Typescript.Generate where

import           Data.Maybe
import           Data.Monoid      ((<>))
import           Data.Text
import           GenericMappings
import           Typescript.Types

class GenerateTypescript a where
  toTypescript :: a -> Text

instance GenerateTypescript TSType where
  toTypescript (TSPrimitiveType tsPrim) = toTypescript tsPrim
  toTypescript (TSInterface iName fields') =
      ("interface " <> iName <> " { \n"
      <> toTypescript fields'
      <> "}"
      )

printTS :: (TypescriptType a) => a -> Text
printTS tsType' =
        fromMaybe ""
    $   toTypescript
    <$> toTypescriptType tsType'



--data InterfaceFieldTraverse =
--  InterfaceFieldTraverse
--    {_typeNameToPrint       :: Text
--    ,_mbAnotherTypeToPrint  :: (Maybe TSType)
--    }
--
--traverseFields :: [TSField] -> InterfaceFieldTraverse
--traverseFields fields' =
--  fmap travField fields'
--  where
--    travField (TSField fieldName tType) =
--      InterfaceFieldTraverse fieldName
--         $ case tType of
--             TSInterface n fs -> Just $ TSInterface n fs
--             _ -> Nothing
--
--genInterface (TSInterface iName fields') =


instance GenerateTypescript TSField where
  toTypescript (TSField (FieldName fieldName') fieldType') =
      fieldName' <> " : " <> typeName
      where
        typeName =
          case fieldType' of
              TSPrimitiveType primitive -> toTypescript primitive
              TSInterface iName _       -> iName

instance GenerateTypescript [TSField] where
  toTypescript []     = ""
  toTypescript (x:xs) = "   " <> toTypescript x <> " \n" <> toTypescript xs

instance GenerateTypescript TSPrimitive where
  toTypescript TSNumber           = "number"
  toTypescript TSString           = "string"
  toTypescript (TSOption tsType') = toTypescript tsType' <> " | null "


genTypescript :: (GenerateTypescript a) => [a] -> Text
genTypescript [] = ""
genTypescript (x:xs) =
   (toTypescript x) <> "\n" <> genTypescript xs
