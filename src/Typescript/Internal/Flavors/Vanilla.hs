module Typescript.Internal.Flavors.Vanilla where

import           Typescript.Internal.Intermediate.Lang
import           Typescript.Internal.Output.Foreign.Class
import           Typescript.Internal.Output.Foreign.TSDefaults

data Vanilla

instance IsForeignType (TSComposite Vanilla) where
    toForeignType (TSCollectionRef tsCollection) =
        defaultForeignArray tsCollection
    toForeignType (TSOptionRef tsOption) = defaultOption tsOption
    toForeignType (TSStructuredType typeName tsStructure) = case tsStructure of
        TSUnionLike tsUnion -> defaultForeignUnion typeName tsUnion
        TSRecordLike tsData -> mkTSInterface typeName tsData


