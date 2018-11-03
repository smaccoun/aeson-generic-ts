module Internal.Intermediate.Typescript.Lang where

import           Data.Text

{-
   MASTER TYPE
-}
data TSIntermediate f =
    TSPrimitiveType TSPrimitive
  | TSCompositeType (TSComposite f)

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
  | TSOption (TSIntermediate f)
  | TSUnionRef Text [TSIntermediate f]


newtype TSArray f = TSArray (TSIntermediate f)


{-
  Typescript "Data types". Classes are an alternative rep to Interface
-}
data TSData f =
    TSData Text [TSField f]

data TSField f =
  TSField
    {fieldName :: FieldName
    ,fieldType :: TSIntermediate f
    }

newtype FieldName = FieldName Text
