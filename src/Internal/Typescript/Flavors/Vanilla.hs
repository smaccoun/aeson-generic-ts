module Internal.Typescript.Flavors.Vanilla where

import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign.Class
import           Internal.Output.Foreign.TSDefaults

data Vanilla

instance IsForeignType (TSComposite Vanilla) where
  toForeignType (TSCollectionRef tsCollection) = defaultForeignArray tsCollection
  toForeignType (TSOptionRef tsOption)  = defaultOption tsOption
  toForeignType (TSStructuredType tsStructure) =
    case tsStructure of
      TSUnionLike tsUnion -> defaultForeignUnion tsUnion
      TSRecordLike tsData -> mkTSInterface tsData


