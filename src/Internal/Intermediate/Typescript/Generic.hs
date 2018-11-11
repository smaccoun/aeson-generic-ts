{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Internal.Intermediate.Typescript.Generic where

import           Data.Proxy
import           Data.Text
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

{-| Instances
-}


instance (Datatype d, GenericTSIntermediate f1, GenericTSIntermediate f2)
  => GenericTSIntermediate
          (D1 d (
            (C1 c1 (S1 ('MetaSel 'Nothing a b 'DecidedLazy) f1))
        :+: (C1 c2 (S1 ('MetaSel 'Nothing a2 b2 'DecidedLazy) f2))
            )
          ) where
  genericToTS datatype =
    TSCompositeType
    $ TSStructuredType typeName
    $ TSUnionLike
    $ TSUnion
       [genericToTS (undefined :: f1 p)
       ,genericToTS (undefined :: f2 p)
       ]
    where
      typeName = pack (datatypeName datatype)

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
