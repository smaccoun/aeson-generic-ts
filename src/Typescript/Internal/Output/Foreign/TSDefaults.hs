{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Typescript.Internal.Output.Foreign.TSDefaults where

import           Data.Text
import qualified Data.Text                                as T

import           Typescript.Internal.Intermediate.Lang
import           Typescript.Internal.Output.Foreign.Class

  {-
  DEFAULT FOREIGN INSTANCES
-}instance IsForeignType (TSComposite f)
    => IsForeignType (TSIntermediate f) where
    toForeignType (TSPrimitiveType prim) = toForeignType prim
    toForeignType (TSCompositeType composite) = toForeignType composite

instance IsForeignType TSPrimitive where
    toForeignType TSString = selfRefForeign "string"
    toForeignType TSNumber = selfRefForeign "number"
    toForeignType TSBoolean = selfRefForeign "boolean"
    toForeignType TSVoid = selfRefForeign "void"

showField :: (IsForeignType (TSIntermediate f)) => TSField f -> Text
showField (TSField (FieldName fName) fType) = fName <> " : "
    <> (refName . toForeignType) fType

showFields :: (IsForeignType (TSIntermediate f)) => [TSField f] -> Text
showFields fields = T.intercalate "\n" $
    fmap (\f -> "  " <> showField f) fields

defaultForeignArray :: (IsForeignType (TSIntermediate f))
    => TSCollection f -> ForeignType
defaultForeignArray (TSCollection tsType') =
    ForeignType { refName     = "Array<" <> rep <> ">"
                , declaration = "Array<" <> rep <> ">"
                }
  where
    rep = refName . toForeignType $ tsType'

defaultForeignUnion :: (IsForeignType (TSIntermediate f))
    => Text -> TSUnion f -> ForeignType
defaultForeignUnion unionName (TSUnion tsTypes') =
    ForeignType { refName     = unionName
                , declaration = "type " <> unionName <> " = " <> ns
                }
  where
    ns = intercalate " | " $ fmap (refName . toForeignType) tsTypes'

defaultOption :: (IsForeignType (TSIntermediate f)) => TSOption f -> ForeignType
defaultOption (TSOption tsType') =
    selfRefForeign ((refName . toForeignType $ tsType') <> " | null ")

mkTSInterface :: (IsForeignType (TSIntermediate f))
    => Text -> TSRecord f -> ForeignType
mkTSInterface iName (TSRecord fields') =
    ForeignType { refName     = iName
                , declaration = "interface " <> iName <> " { \n"
                      <> showFields fields' <> "\n}"
                }

