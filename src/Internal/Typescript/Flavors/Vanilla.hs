module Internal.Typescript.Flavors.Vanilla where

import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign.Class
import           Internal.Output.Foreign.TSDefaults    (defaultForeignArray, defaultForeignUnion)

data Vanilla

instance IsForeignType (TSComposite Vanilla) where
  toForeignType (TSCollectionRef tsCollection) = TSCollectionRef <$> defaultForeignArray tsCollection
  toForeignType (TSUnionRef tsUnion) = TSUnionRef <$> defaultForeignUnion tsUnion
  toForeignType (TSOptionRef tsOption)  = TSOptionRef <$> toForeignType tsOption
  toForeignType (TSDataType tsData) = TSDataType <$> toForeignType tsData

instance IsForeignType (TSOption Vanilla) where
  toForeignType (TSOption tsType') =
    selfRefForeign ((refName . toForeignType $ tsType') <> " | null ")
