module Spec where

import           FpTsSpec
import           Test.Hspec
import           VanillaSpec


aesonGenericTSSpec :: Spec
aesonGenericTSSpec = do
  FpTsSpec.spec
  VanillaSpec.spec

