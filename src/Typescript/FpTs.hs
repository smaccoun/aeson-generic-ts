module Typescript.FpTs where

import           Bridge.Intermediate
import           Data.Text
import           Typescript.Types

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


