module Typescript.Types where

import           Bridge.Intermediate
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
  toForeignType (TSPrimitiveType prim) = toForeignType prim
  toForeignType (TSCompositeType composite) = toForeignType composite
  toForeignType (TSCustomizableType tsCustom) = toForeignType tsCustom


{-
  Typescript Primitives. Have a default rep
-}
data TSPrimitive =
    TSNumber
  | TSString
  | TSBoolean
    deriving (Eq, Show)

instance IsForeignType TSPrimitive where
  toForeignType TSString  = "string"
  toForeignType TSNumber  = "number"
  toForeignType TSBoolean = "boolean"


{-
  Composite Types
-}
data TSComposite f =
    TSCollection (TSArray f)
  | TSDataType (TSData f)

data TSArray f = TSArray (TSType f)

defaultForeignArray :: (IsForeignType (TSType f)) => TSArray f -> Text
defaultForeignArray (TSArray tsType') = "Array<" <> toForeignType tsType' <> ">"

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

instance (IsForeignType (TSType f)) => IsForeignType (TSField f) where
  toForeignType (TSField (FieldName fName) fType) = fName <> " : " <> toForeignType fType

instance (IsForeignType (TSType f)) => IsForeignType [TSField f] where
  toForeignType fields = T.intercalate "\n" $ fmap toForeignType fields

instance (IsForeignType (TSType f)) => IsForeignType (TSInterface f) where
  toForeignType (TSInterface iName fields') =
      ("interface " <> iName <> " { \n"
      <> toForeignType fields'
      <> "}"
      )

{-
  Custom types that often have many representations
-}
data TSCustom f =
    TSOption (TSType f)
  | TSUnionRef Text [TSType f]


