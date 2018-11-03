module Internal.Intermediate.Typescript.Lang where

import           Data.Text

{-
   MASTER TYPE
-}
data TSIntermediate f =
    TSPrimitiveType TSPrimitive
  | TSCompositeType (TSComposite f)
  | TSCustomizableType (TSCustom f)


{-
  Typescript Primitives. Have a default rep
-}
data TSPrimitive =
    TSNumber
  | TSString
  | TSBoolean
    deriving (Eq, Show)


{-
  Composite Types
-}
data TSComposite f =
    TSCollection (TSArray f)
  | TSDataType (TSData f)

newtype TSArray f = TSArray (TSIntermediate f)


{-
  Typescript "Data types". Classes are an alternative rep to Interface
-}
newtype TSData f =
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
    ,fieldType :: TSIntermediate f
    }

newtype FieldName = FieldName Text

{-
  Custom types that often have many representations
-}
data TSCustom f =
    TSOption (TSIntermediate f)
  | TSUnionRef Text [TSIntermediate f]
