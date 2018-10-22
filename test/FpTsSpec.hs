module FpTsSpec where

import           Bridge.Generics
import           Data.Proxy
import           Data.Text          (Text)
import           GHC.Generics
import           PrintForeign
import           Test.Hspec
import           Typescript.FpTs

newtype AnOption = AnOption (Maybe Text) deriving (Generic, BridgeType)

spec :: Spec
spec = do
  describe "option_type" $ do
    it "handles_a_simple_option" $ do
      ts <- printFromBridge (Proxy :: Proxy FpTs) (Proxy :: Proxy AnOption)
      ts `shouldBe` knownSolution
      where
        knownSolution = "Option<string>"
