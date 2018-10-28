module Internal.Typescript.Flavors.Vanilla where

import           Data.Text
import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign

data Vanilla = Vanilla

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
