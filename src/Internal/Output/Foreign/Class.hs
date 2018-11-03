{-# LANGUAGE DeriveGeneric, DeriveFunctor, DeriveTraversable #-}

module Internal.Output.Foreign.Class where

import Data.Text
import GHC.Generics

{-|
  Instantiate this class for all types that will be printed out as Text.
-}
class IsForeignType t where
  toForeignType :: t -> ForeignType

{-|
  A type that represents a reference and a declaration.
  The reference is used in data structures referring to this type,
  while the declaration represent the full declaration of the type. For example, here the data type R makes reference to another data type S. The second line is the declaration for S, and the type ref in R is the refName.

> data R = R {sField :: S}
> data S = S {a      :: Int}
-}
data ForeignType =
  ForeignType
    {refName     :: Text
    ,declaration :: Text
    } deriving (Generic)

data TypescriptOutput =
  TypescriptOutput
    {typeOutput :: ForeignType
    ,mbRequiresLib :: Maybe TSLibrary
    }

class (IsForeignType t) => OutputsTypescript t where
  toTypescriptOutput :: t -> TypescriptOutput

newtype TSLibrary = TSLibrary Text

mkTypescriptOut :: (IsForeignType t) => Maybe TSLibrary -> t -> TypescriptOutput
mkTypescriptOut mbLib foreignType =
  TypescriptOutput (toForeignType foreignType) mbLib

selfRefForeign :: Text -> ForeignType
selfRefForeign ref = ForeignType ref ref
