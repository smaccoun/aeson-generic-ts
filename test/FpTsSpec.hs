module FpTsSpec where

import           Internal.Intermediate.Bridge.Generics
import           Data.Proxy
import           Data.Text          (Text)
import           GHC.Generics
import           Internal.Output.PrintForeign
import           Test.Hspec
import           Internal.Typescript.Flavors.FpTs

newtype AnOption = AnOption (Maybe Text) deriving (Generic, BridgeType)

spec :: Spec
spec = do
  describe "option_type" $ do
    it "handles_a_simple_option" $ do
      ts <- printFromBridge (Proxy :: Proxy FpTs) (Proxy :: Proxy AnOption)
      ts `shouldBe` knownSolution
  where knownSolution = "Option<string>"
