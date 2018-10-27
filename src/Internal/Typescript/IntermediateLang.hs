module Internal.Typescript.IntermediateLang where

import           Internal.Intermediate.Bridge.Lang
import           Data.Text
import qualified Data.Text           as T

{-
   MASTER TYPE
-}
data TSType f =
    TSPrimitiveType TSPrimitive
  | TSCompositeType (TSComposite f)
  | TSCustomizableType (TSCustom f)

instance (IsForeignType (TSCustom f), IsForeignType (TSComposite f))  => IsForeignType (TSType f) where
  toForeignType (TSPrimitiveType prim) = TSPrimitiveType <$> toForeignType prim
  toForeignType (TSCompositeType composite) = TSCompositeType <$> toForeignType composite
  toForeignType (TSCustomizableType tsCustom) = TSCustomizableType <$> toForeignType tsCustom




{-
  Typescript Primitives. Have a default rep
-}
data TSPrimitive =
    TSNumber
  | TSString
  | TSBoolean
    deriving (Eq, Show)

instance IsForeignType TSPrimitive where
  toForeignType TSString  = selfRefForeign "string"
  toForeignType TSNumber  = selfRefForeign "number"
  toForeignType TSBoolean = selfRefForeign "boolean"


{-
  Composite Types
-}
data TSComposite f =
    TSCollection (TSArray f)
  | TSDataType (TSData f)

data TSArray f = TSArray (TSType f)

defaultForeignArray :: (IsForeignType (TSType f)) => TSArray f -> Text
defaultForeignArray (TSArray tsType') =
  "Array<" <> rep <> ">"
  where
    rep = refName . toForeignType $ tsType'

{-
  Typescript "Data types". Classes are an alternative rep to Interface
-}
data TSData f =
    TSInterfaceRef (TSInterface f)
--  | TSClassRef f

data TSInterface f =
  TSInterface
    {interfaceName :: Text
    ,fields        :: [TSField f]
    }

data TSField f =
  TSField
    {fieldName :: FieldName
    ,fieldType :: (TSType f)
    }

newtype FieldName = FieldName Text

showField :: (IsForeignType (TSType f)) => TSField f -> Text
showField (TSField (FieldName fName) fType) = fName <> " : " <> (refName . toForeignType) fType

showFields ::  (IsForeignType (TSType f)) => [TSField f] -> Text
showFields fields = T.intercalate "\n" $ fmap (\f -> "  " <> showField f) fields

instance (IsForeignType (TSType f)) => IsForeignType (TSInterface f) where
  toForeignType (TSInterface iName fields') =
    ForeignType
      {refName     = iName
      ,declaration =
          ("interface " <> iName <> " { \n"
          <> showFields fields'
          <> "\n}"
          )
      }

{-
  Custom types that often have many representations
-}
data TSCustom f =
    TSOption (TSType f)
  | TSUnionRef Text [TSType f]


