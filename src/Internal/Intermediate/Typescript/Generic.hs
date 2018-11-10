{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Internal.Intermediate.Typescript.Generic where

import GHC.Generics
import Internal.Intermediate.Typescript.Lang
import Data.Text
import Data.Proxy

class Typescript a where
  toTSIntermediate :: a -> TSIntermediate flavor
  default toTSIntermediate :: (Generic a, GenericTSIntermediate (Rep a)) => a -> TSIntermediate flavor
  toTSIntermediate = genericToTS . from

class GenericTSIntermediate hkf where
  genericToTS :: hkf a -> TSIntermediate flavor

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
