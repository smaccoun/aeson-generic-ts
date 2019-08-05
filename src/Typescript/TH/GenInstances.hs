-- | Quick helper functions for (recursively) generating typescript instances using template haskell

{-# LANGUAGE TemplateHaskell #-}

module Typescript.TH.GenInstances where

import Language.Haskell.TH.ReifyMany
import Typescript.Internal.Intermediate.Generic (TypescriptType)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (DerivStrategy(..))

deriveTypescriptTypesRecursively :: [Name] -> Q [Dec]
deriveTypescriptTypesRecursively nms = do
  names <- reifyManyWithoutInstances ''TypescriptType nms (const True)
  let allInstances = deriveTSInstance <$> names
  pure allInstances

deriveTSInstance :: Name -> Dec
deriveTSInstance nm = StandaloneDerivD (Just AnyclassStrategy) [] (ConT nm)
