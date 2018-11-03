module Internal.Typescript.Flavors.FpTs where

import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign.Class
import           Internal.Output.Foreign.TSDefaults    (defaultForeignArray, defaultForeignUnion)

data FpTs

instance IsForeignType (TSComposite FpTs) where
  toForeignType (TSCollectionRef tsCollection) = TSCollectionRef <$> defaultForeignArray tsCollection
  toForeignType (TSUnionRef tsUnion) = TSUnionRef <$> defaultForeignUnion tsUnion
  toForeignType (TSOptionRef tsOption)  = TSOptionRef <$> toForeignType tsOption
  toForeignType (TSDataType tsData) = TSDataType <$> toForeignType tsData

instance IsForeignType (TSUnion FpTs)  where
  toForeignType = defaultForeignUnion

instance IsForeignType (TSOption FpTs) where
  toForeignType (TSOption tsType') =
    selfRefForeign $ "Option<" <> (refName . toForeignType $ tsType') <> ">"

instance IsForeignType (TSCollection FpTs) where
  toForeignType = defaultForeignArray



