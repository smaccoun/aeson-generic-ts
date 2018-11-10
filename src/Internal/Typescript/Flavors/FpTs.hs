module Internal.Typescript.Flavors.FpTs where

import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign.Class
import           Internal.Output.Foreign.TSDefaults

data FpTs

instance IsForeignType (TSComposite FpTs) where
  toForeignType (TSCollectionRef tsCollection) = defaultForeignArray tsCollection
  toForeignType (TSOptionRef tsOption) = mkFpTSOption tsOption
  toForeignType (TSStructuredType tsStructure) =
    case tsStructure of
      TSUnionLike tsUnion -> defaultForeignUnion tsUnion
      TSRecordLike tsData -> mkTSInterface tsData

mkFpTSOption :: (IsForeignType (TSIntermediate f)) => TSOption f -> ForeignType
mkFpTSOption (TSOption tsType') =
  selfRefForeign $ "Option<" <> (refName . toForeignType $ tsType') <> ">"

instance OutputsTypescript (TSIntermediate FpTs) where
  toTypescriptOutput = mkTypescriptOut (Just (TSLibrary "fp-ts"))



