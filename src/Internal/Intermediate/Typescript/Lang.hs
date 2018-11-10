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
    TSCollectionRef (TSCollection f)
  | TSOptionRef (TSOption f)
  | TSStructuredType (TSStructured f)

newtype TSCollection f = TSCollection (TSIntermediate f)

newtype TSOption f = TSOption (TSIntermediate f)

data TSUnion f = TSUnion Text [TSIntermediate f]

{-
  Typescript "Data types". Classes are an alternative rep to Interface
-}

data TSStructured f =
    TSRecordLike (TSRecord f)
  | TSUnionLike (TSUnion f)

data TSRecord f =
    TSRecord Text [TSField f]

data TSField f =
  TSField
    {fieldName :: FieldName
    ,fieldType :: TSIntermediate f
    }

newtype FieldName = FieldName Text
