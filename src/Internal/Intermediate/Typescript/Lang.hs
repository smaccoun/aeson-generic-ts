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
  | TSStructuredType Text (TSStructured f)

newtype TSCollection f = TSCollection (TSIntermediate f)

newtype TSOption f = TSOption (TSIntermediate f)

data TSUnion f = TSUnion [TSIntermediate f]

instance Semigroup (TSUnion f) where
  (TSUnion l1) <> (TSUnion l2) = TSUnion $ l1 <> l2

{-
  Typescript "Data types", which are largely structural but can also be transformed into classes.
-}

data TSStructured f =
    TSRecordLike (TSRecord f)
  | TSUnionLike (TSUnion f)

data TSRecord f =
    TSRecord [TSField f]

data TSField f =
  TSField
    {fieldName :: FieldName
    ,fieldType :: TSIntermediate f
    }

newtype FieldName = FieldName Text
