module Internal.Typescript.Flavors.FpTs where

import           Data.Text
import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign

data FpTs = FpTs

instance IsForeignType (TSComposite FpTs) where
  toForeignType (TSCollection tar) = TSCollection <$> toForeignType tar
  toForeignType (TSDataType (TSInterfaceRef tsInterface)) = TSDataType <$> TSInterfaceRef <$> toForeignType tsInterface

instance IsForeignType (TSArray FpTs) where
  toForeignType tsArray =
    ForeignType
      {refName = asDefault
      ,declaration = asDefault
      }
   where
     asDefault = defaultForeignArray tsArray

instance IsForeignType (TSCustom FpTs) where
  toForeignType (TSOption tsType') =
    selfRefForeign $ "Option<" <> (refName . toForeignType $ tsType') <> ">"
  toForeignType (TSUnionRef unionName tsTypes') =
    ForeignType
      {refName = unionName
      ,declaration =  "type " <> (unionName) <> " = " <> ns
      }
    where
      ns =
         intercalate " | "
       $ fmap (refName . toForeignType) tsTypes'

