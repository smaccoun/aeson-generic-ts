module Internal.Typescript.Flavors.FpTs where

import           Data.Text
import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign.Class
import           Internal.Output.Foreign.TSDefaults    (defaultForeignArray)

data FpTs

instance IsForeignType (TSComposite FpTs) where
  toForeignType (TSCollectionRef tar) = TSCollectionRef <$> toForeignType tar
  toForeignType (TSDataType tsData) = TSDataType <$> toForeignType tsData
  toForeignType (TSOptionRef tsType') = TSOptionRef <$> toForeignType tsType'
  toForeignType (TSUnionRef unionName tsTypes') =
    ForeignType
      {refName = unionName
      ,declaration =  "type " <> unionName <> " = " <> ns
      }
    where
      ns =
          intercalate " | "
        $ fmap (refName . toForeignType) tsTypes'


instance IsForeignType (TSOption FpTs) where
  toForeignType (TSOption tsType') =
    selfRefForeign $ "Option<" <> (refName . toForeignType $ tsType') <> ">"

instance IsForeignType (TSCollection FpTs) where
  toForeignType tsArray =
    ForeignType
      {refName = asDefault
      ,declaration = asDefault
      }
   where
     asDefault = defaultForeignArray tsArray



