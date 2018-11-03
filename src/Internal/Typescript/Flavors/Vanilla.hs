module Internal.Typescript.Flavors.Vanilla where

import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign.Class
import           Internal.Output.Foreign.TSDefaults    (defaultForeignArray, defaultForeignUnion)

data Vanilla

instance IsForeignType (TSComposite Vanilla) where
  toForeignType (TSCollectionRef tsCollection) = defaultForeignArray tsCollection
  toForeignType (TSUnionRef tsUnion) = defaultForeignUnion tsUnion
  toForeignType (TSOptionRef tsOption)  = defaultOption tsOption
  toForeignType (TSDataType tsData) = toForeignType tsData


defaultOption :: TSOption Vanilla -> ForeignType
defaultOption (TSOption tsType') =
  selfRefForeign ((refName . toForeignType $ tsType') <> " | null ")
