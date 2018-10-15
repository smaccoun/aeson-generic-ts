module Typescript.Vanilla where

import           Bridge.Intermediate
import           Data.Text
import           Typescript.Types

data Vanilla

instance IsForeignType (TSComposite Vanilla) where
  toForeignType (TSCollection tar) = TSCollection <$> toForeignType tar
  toForeignType (TSDataType (TSInterfaceRef tsInterface)) = TSDataType <$> TSInterfaceRef <$> toForeignType tsInterface

instance IsForeignType (TSArray Vanilla) where
  toForeignType tsArray =
    ForeignType
      {refName = asDefault
      ,declaration = asDefault
      }
   where
     asDefault = defaultForeignArray tsArray

instance IsForeignType (TSCustom Vanilla) where
  toForeignType (TSOption tsType') =
    selfRefForeign ((refName . toForeignType $ tsType') <> " | null ")
  toForeignType (TSUnionRef unionName tsTypes') =
    ForeignType
      {refName = unionName
      ,declaration =  "type " <> (unionName) <> " = " <> ns
      }
    where
      ns =
         intercalate " | "
       $ fmap (refName . toForeignType) tsTypes'


--data GenMany = forall a . TypescriptType a => GenMany a
--
--genTypescript :: [GenMany] -> Text
--genTypescript [] = ""
--genTypescript ((GenMany x):xs) =
--   (printTS x) <> "\n" <> genTypescript xs
--
--printTS :: (TypescriptType a) => a -> Text
--printTS tsType' =
--        fromMaybe ""
--    $   toTypescript
--    <$> toTypescriptType tsType'
