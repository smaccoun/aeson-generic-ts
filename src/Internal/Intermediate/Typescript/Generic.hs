{-# LANGUAGE DefaultSignatures #-}

module Internal.Intermediate.Typescript.Generic where

import GHC.Generics
import Internal.Intermediate.Typescript.Lang
import Data.Text

class Typescript a where
  toTSIntermediate :: a -> TSIntermediate flavor
  default toTSIntermediate :: (Generic a, GenericTSIntermediate (Rep a)) => a -> TSIntermediate flavor
  toTSIntermediate = genericToTS . from

class GenericTSIntermediate hkf where
  genericToTS :: hkf a -> TSIntermediate flavor

instance Typescript Text where
  toTSIntermediate _ = TSPrimitiveType TSString

instance (Num a) => Typescript a where
  toTSIntermediate _ = TSPrimitiveType TSNumber

instance Typescript Bool where
  toTSIntermediate _ = TSPrimitiveType TSBoolean
