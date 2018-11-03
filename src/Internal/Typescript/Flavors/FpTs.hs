module Internal.Typescript.Flavors.FpTs where

import           Data.Text
import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign.Class
import           Internal.Output.Foreign.TSDefaults    (defaultForeignArray)

data FpTs

instance IsForeignType (TSComposite FpTs) where
  toForeignType (TSCollection tar) = TSCollection <$> toForeignType tar
  toForeignType (TSDataType tsData) = TSDataType <$> toForeignType tsData
  toForeignType (TSOption tsType') =
    selfRefForeign $ "Option<" <> (refName . toForeignType $ tsType') <> ">"
  toForeignType (TSUnionRef unionName tsTypes') =
    ForeignType
      {refName = unionName
      ,declaration =  "type " <> unionName <> " = " <> ns
      }
    where
      ns =
          intercalate " | "
        $ fmap (refName . toForeignType) tsTypes'


instance IsForeignType (TSArray FpTs) where
  toForeignType tsArray =
    ForeignType
      {refName = asDefault
      ,declaration = asDefault
      }
   where
     asDefault = defaultForeignArray tsArray



