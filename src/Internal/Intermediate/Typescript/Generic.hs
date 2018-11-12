{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Internal.Intermediate.Typescript.Generic where

import           Data.Proxy
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           GHC.Generics
import           Internal.Intermediate.Typescript.Lang

class Typescript a where
  toTSIntermediate :: a -> TSIntermediate flavor
  default toTSIntermediate :: (Generic a, GenericTSIntermediate (Rep a)) => a -> TSIntermediate flavor
  toTSIntermediate = genericToTS . from

{-| Helper typeclasses for intermediate translations
-}
class GenericTSIntermediate hkf where
  genericToTS :: hkf a -> TSIntermediate flavor

class GenericTSStructured hkf where
  toTSStructured :: hkf a -> TSStructured flavor

class GenericTSUnion hkf where
  toTSUnion :: hkf a -> TSUnion flavor

class GenericTSFields hkf where
  toTSFields :: hkf a -> [TSField flavor]

{-| Instances
-}


instance (GenericTSFields f, GenericTSFields g) => GenericTSFields (f :*: g) where
  toTSFields _ = f1 <> f2
    where
      f1 = toTSFields (undefined :: f p)
      f2 = toTSFields (undefined :: g p)

instance (GenericTSIntermediate f, Selector s) => GenericTSFields (S1 s f) where
  toTSFields s =
    [TSField
      {fieldName = FieldName $ T.pack $ selName s
      ,fieldType = genericToTS $ unM1 s
      }
    ]

instance (Datatype d, GenericTSFields f, GenericTSFields s) => GenericTSIntermediate (D1 d (C1 c (s :*: f))) where
  genericToTS d =
    TSCompositeType
    $ TSStructuredType typeName
    $ TSRecordLike
    $ TSRecord $ toTSFields (unM1 . unM1 $ d)
    where
      typeName = T.pack (datatypeName d)

instance (Datatype d, GenericTSIntermediate f, Selector s) => GenericTSIntermediate (D1 d (C1 c (S1 s f))) where
  genericToTS d =
    TSCompositeType
    $ TSStructuredType typeName
    $ TSRecordLike
    $ TSRecord $ toTSFields (unM1 . unM1 $ d)
    where
      typeName = T.pack (datatypeName d)

instance (GenericTSIntermediate f1)
  => GenericTSUnion (C1 c1 (S1 ('MetaSel 'Nothing a b 'DecidedLazy) f1)) where
  toTSUnion _ =
    TSUnion
       [genericToTS (undefined :: f1 p)]

instance (GenericTSUnion f, GenericTSUnion g) => GenericTSUnion (f :+: g) where
  toTSUnion _ = u1 <> u2
    where
      u1 = toTSUnion (undefined :: f p)
      u2 = toTSUnion (undefined :: g p)


instance (Datatype d, GenericTSUnion s, GenericTSUnion f)
  => GenericTSIntermediate (D1 d (s :+: f)) where
  genericToTS datatype =
    TSCompositeType
    $ TSStructuredType typeName
    $ TSUnionLike
    $  toTSUnion (unM1 datatype)
    where
      typeName = T.pack (datatypeName datatype)

instance (Datatype d, GenericTSIntermediate f1, GenericTSIntermediate f2)
  => GenericTSIntermediate
          (D1 d (
            C1 c1 (S1 ('MetaSel 'Nothing a b 'DecidedLazy) f1)
        :*: C1 c2 (S1 ('MetaSel 'Nothing a2 b2 'DecidedLazy) f2)
            )
          ) where
  genericToTS datatype =
    TSCompositeType
    $ TSStructuredType typeName
    $ TSRecordLike
    $ TSRecord [f1, f2]
    where
      typeName = T.pack (datatypeName datatype)
      f1 = TSField (FieldName "meow") (genericToTS (undefined :: f1 p))
      f2 = TSField (FieldName "f2") (genericToTS (undefined :: f2 p))


instance Typescript t => GenericTSIntermediate (Rec0 t) where
  genericToTS _ = toTSIntermediate (Proxy :: Proxy t)

instance (GenericTSStructured f, Constructor c)  => GenericTSStructured (C1 c f) where
  toTSStructured c = toTSStructured (unM1 c)

instance (GenericTSIntermediate f, Selector s)  => GenericTSStructured (S1 s f) where
  toTSStructured s = TSUnionLike $ TSUnion [genericToTS (unM1 s)]




instance (GenericTSIntermediate f, GenericTSIntermediate g) => GenericTSStructured (f :+: g) where
  toTSStructured _ =
    TSUnionLike $ TSUnion
      [genericToTS (undefined :: f p)
      ,genericToTS (undefined :: g p)
      ]

instance Typescript a => Typescript (Proxy a) where
  toTSIntermediate _ = toTSIntermediate (undefined :: a)

instance Typescript t => Typescript (Maybe t) where
  toTSIntermediate _ = TSCompositeType $ TSOptionRef $ TSOption (toTSIntermediate (Proxy :: Proxy t))

instance Typescript t => Typescript [t] where
  toTSIntermediate _ = TSCompositeType $ TSCollectionRef $ TSCollection (toTSIntermediate (Proxy :: Proxy t))

instance Typescript Text where
  toTSIntermediate _ = TSPrimitiveType TSString

instance Typescript Int where
  toTSIntermediate _ = TSPrimitiveType TSNumber

instance Typescript Bool where
  toTSIntermediate _ = TSPrimitiveType TSBoolean
