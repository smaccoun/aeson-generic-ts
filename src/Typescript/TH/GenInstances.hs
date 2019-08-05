-- | Quick helper functions for (recursively) generating typescript instances using template haskell

{-# LANGUAGE TemplateHaskell #-}

module Typescript.TH.GenInstances where

import Language.Haskell.TH.ReifyMany
import Typescript.Internal.Intermediate.Generic (TypescriptType)
import Language.Haskell.TH
--import Language.Haskell.TH.Syntax (DerivStrategy(..))

deriveTypescriptTypesRecursively :: [Name] -> Q [Dec]
deriveTypescriptTypesRecursively nms = do
  names <- reifyManyWithoutInstances ''TypescriptType nms (const True)
  concat <$> mapM deriveTSInstance names

deriveTSInstance :: Name -> Q [Dec]
deriveTSInstance nm = do
  [d|
     deriving instance TypescriptType $(conT nm)
   |]
