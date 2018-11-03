{-# LANGUAGE DefaultSignatures #-}

module Internal.Intermediate.Typescript.Generic where

import GHC.Generics
import Internal.Intermediate.Typescript.Lang
import Data.Text
import Data.Proxy

class Typescript a where
  toTSType :: a -> TSType flavor
  default toTSType :: (Generic a, GenericTSType (Rep a)) => a -> TSType flavor
  toTSType = genericToTS . from

class GenericTSType hkf where
  genericToTS :: hkf a -> TSType flavor

instance Typescript Text where
  toTSType _ = TSPrimitiveType TSString
