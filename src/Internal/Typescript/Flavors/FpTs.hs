module Internal.Typescript.Flavors.FpTs where

import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign.Class
import           Internal.Output.Foreign.TSDefaults

data FpTs

instance IsForeignType (TSComposite FpTs) where
  toForeignType (TSCollectionRef tsCollection) = defaultForeignArray tsCollection
  toForeignType (TSUnionRef tsUnion) = defaultForeignUnion tsUnion
  toForeignType (TSOptionRef (TSOption tsType')) =
    selfRefForeign $ "Option<" <> (refName . toForeignType $ tsType') <> ">"
  toForeignType (TSDataType tsData) = mkTSInterface tsData



