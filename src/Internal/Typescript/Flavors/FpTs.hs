module Internal.Typescript.Flavors.FpTs where

import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign.Class
import           Internal.Output.Foreign.TSDefaults

data FpTs

instance IsForeignType (TSComposite FpTs) where
  toForeignType (TSCollectionRef tsCollection) = defaultForeignArray tsCollection
  toForeignType (TSUnionRef tsUnion) = defaultForeignUnion tsUnion
  toForeignType (TSOptionRef tsOption) = mkFpTSOption tsOption
  toForeignType (TSDataType tsData) = mkTSInterface tsData

mkFpTSOption :: (IsForeignType (TSIntermediate f)) => TSOption f -> ForeignType
mkFpTSOption (TSOption tsType') =
  selfRefForeign $ "Option<" <> (refName . toForeignType $ tsType') <> ">"

instance IsForeignType (TSOption FpTs) where
  toForeignType = mkFpTSOption

instance OutputsTypescript (TSOption FpTs) where
  toTypescriptOutput = mkTypescriptOut (Just (TSLibrary "fpts"))



