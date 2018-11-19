module Typescript.Internal.Output.PrintForeign where

import           Data.Proxy
import           Data.Text                                (Text)
import           Typescript.Internal.Intermediate.Generic
import           Typescript.Internal.Intermediate.Lang
import           Typescript.Internal.Output.Foreign.Class

{-| Core function for outputting typescript

Simple example using the `Vanilla` flavor

-}
-- |
-- >>> import Typescript.Internal.Flavors.Vanilla
-- >>> declaration $ foreignTypescript (Proxy :: Proxy Vanilla) (Proxy :: Proxy Int)
-- "number"
foreignTypescript
  :: (TypescriptType hsType, IsForeignType (TSIntermediate flavor))
  => Proxy flavor
  -> Proxy hsType
  -> ForeignType
foreignTypescript pFlavor tsType' = toForeignType $ toTSFlavor pFlavor tsType'

mkTypescriptDeclaration
  :: (TypescriptType hsType, IsForeignType (TSIntermediate flavor))
  => Proxy flavor
  -> Proxy hsType
  -> Text
mkTypescriptDeclaration pFlavor tsType' =
  declaration $ foreignTypescript pFlavor tsType'

toTSFlavor
  :: (TypescriptType hsType)
  => Proxy flavor
  -> Proxy hsType
  -> TSIntermediate flavor
toTSFlavor _ = toTSIntermediate


