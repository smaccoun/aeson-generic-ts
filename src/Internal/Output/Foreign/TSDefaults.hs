{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal.Output.Foreign.TSDefaults where

import           Data.Text
import qualified Data.Text                             as T
import           Internal.Intermediate.Typescript.Lang
import           Internal.Output.Foreign.Class


{-
  DEFAULT FOREIGN INSTANCES
-}

instance IsForeignType (TSComposite f)  => IsForeignType (TSIntermediate f) where
  toForeignType (TSPrimitiveType prim) = toForeignType prim
  toForeignType (TSCompositeType composite) = toForeignType composite


instance IsForeignType TSPrimitive where
  toForeignType TSString  = selfRefForeign "string"
  toForeignType TSNumber  = selfRefForeign "number"
  toForeignType TSBoolean = selfRefForeign "boolean"

showField :: (IsForeignType (TSIntermediate f)) => TSField f -> Text
showField (TSField (FieldName fName) fType) =
  fName <> " : " <> (refName . toForeignType) fType

showFields :: (IsForeignType (TSIntermediate f)) => [TSField f] -> Text
showFields fields =
  T.intercalate "\n" $ fmap (\f -> "  " <> showField f) fields

defaultForeignArray :: (IsForeignType (TSIntermediate f)) => TSCollection f -> ForeignType
defaultForeignArray (TSCollection tsType') =
  ForeignType
    {refName = rep
    ,declaration = "Array<" <> rep <> ">"
    }
    where rep = refName . toForeignType $ tsType'

defaultForeignUnion :: (IsForeignType (TSIntermediate f)) => TSUnion f -> ForeignType
defaultForeignUnion (TSUnion unionName tsTypes') =
    ForeignType
      {refName = unionName
      ,declaration =  "type " <> unionName <> " = " <> ns
      }
    where
      ns =
          intercalate " | "
        $ fmap (refName . toForeignType) tsTypes'

defaultOption :: (IsForeignType (TSIntermediate f)) => TSOption f -> ForeignType
defaultOption (TSOption tsType') =
  selfRefForeign ((refName . toForeignType $ tsType') <> " | null ")

mkTSInterface :: (IsForeignType (TSIntermediate f)) => TSData f -> ForeignType
mkTSInterface (TSData iName fields')=
  ForeignType
    {refName     = iName
    ,declaration =
            "interface " <> iName <> " { \n"
        <> showFields fields'
        <> "\n}"
    }

