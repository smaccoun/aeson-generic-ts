module Typescript.Internal.Flavors.FpTs where

import           Typescript.Internal.Intermediate.Lang
import           Typescript.Internal.Output.Foreign.Class
import           Typescript.Internal.Output.Foreign.TSDefaults

data FpTs

instance IsForeignType (TSComposite FpTs) where
    toForeignType (TSCollectionRef tsCollection) =
        defaultForeignArray tsCollection
    toForeignType (TSOptionRef tsOption) = mkFpTSOption tsOption
    toForeignType (TSStructuredType typeName tsStructure) = case tsStructure of
        TSUnionLike tsUnion -> defaultForeignUnion typeName tsUnion
        TSRecordLike tsData -> mkTSInterface typeName tsData

mkFpTSOption :: (IsForeignType (TSIntermediate f)) => TSOption f -> ForeignType
mkFpTSOption (TSOption tsType') = selfRefForeign $ "Option<"
    <> (refName . toForeignType $ tsType') <> ">"

instance OutputsTypescript (TSIntermediate FpTs) where
    toTypescriptOutput = mkTypescriptOut (Just (TSLibrary "fp-ts"))



