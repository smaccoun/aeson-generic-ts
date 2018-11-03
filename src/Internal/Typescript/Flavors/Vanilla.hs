module Internal.Typescript.Flavors.Vanilla where

import           Data.Text
import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign.Class
import           Internal.Output.Foreign.TSDefaults    (defaultForeignArray)

data Vanilla

instance IsForeignType (TSComposite Vanilla) where
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

instance IsForeignType (TSOption Vanilla) where
  toForeignType (TSOption tsType') =
    selfRefForeign ((refName . toForeignType $ tsType') <> " | null ")

instance IsForeignType (TSCollection Vanilla) where
  toForeignType tsArray =
    ForeignType
      {refName = asDefault
      ,declaration = asDefault
      }
   where
     asDefault = defaultForeignArray tsArray
