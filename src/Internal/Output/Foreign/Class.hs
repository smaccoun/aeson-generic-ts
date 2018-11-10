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
  For example, the following two lines are __declarations__
  The first declaration for 'Foo' uses teh __refName__ of 'Bar' to refer to it in the field `sField`

> data Foo = Foo {sField    :: Bar}
> data Bar = Bar {someField :: Int}
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
