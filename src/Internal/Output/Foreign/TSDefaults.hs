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
  toForeignType (TSPrimitiveType prim) = TSPrimitiveType <$> toForeignType prim
  toForeignType (TSCompositeType composite) = TSCompositeType <$> toForeignType composite


instance IsForeignType TSPrimitive where
  toForeignType TSString  = selfRefForeign "string"
  toForeignType TSNumber  = selfRefForeign "number"
  toForeignType TSBoolean = selfRefForeign "boolean"

instance (IsForeignType (TSIntermediate f)) => IsForeignType (TSData f) where
  toForeignType (TSData iName fields') =
    ForeignType
      {refName     = iName
      ,declaration =
             "interface " <> iName <> " { \n"
          <> showFields fields'
          <> "\n}"
      }


showField :: (IsForeignType (TSIntermediate f)) => TSField f -> Text
showField (TSField (FieldName fName) fType) =
  fName <> " : " <> (refName . toForeignType) fType

showFields :: (IsForeignType (TSIntermediate f)) => [TSField f] -> Text
showFields fields =
  T.intercalate "\n" $ fmap (\f -> "  " <> showField f) fields

defaultForeignArray :: (IsForeignType (TSIntermediate f)) => TSCollection f -> Text
defaultForeignArray (TSCollection tsType') = "Array<" <> rep <> ">"
  where rep = refName . toForeignType $ tsType'
