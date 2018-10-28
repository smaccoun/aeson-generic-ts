{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Internal.Output.Foreign where

import Data.Text
import GHC.Generics

class IsForeignType t where
  toForeignType :: t -> (ForeignType t)

data ForeignType t =
  ForeignType
    {refName     :: Text
    ,declaration :: Text
    } deriving (Generic, Functor)

selfRefForeign :: Text -> ForeignType t
selfRefForeign ref =
  ForeignType ref ref
