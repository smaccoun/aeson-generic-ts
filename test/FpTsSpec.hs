{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FpTsSpec where

import           Internal.Intermediate.Bridge.Generics
import           Data.Proxy
import           Data.Text          (Text)
import           GHC.Generics
import           Internal.Output.PrintForeign
import           Test.Hspec
import           Internal.Typescript.Flavors.FpTs

spec :: Spec
spec = describe "option_type" $ do
  it "handles_a_simple_option" $ do
    ts <- printFpTs (Proxy :: Proxy AnOption)
    ts `shouldBe` knownSolution
  where knownSolution = "Option<string>"


newtype AnOption = AnOption (Maybe Text) deriving (Generic, BridgeType)

printFpTs :: Proxy AnOption -> IO Text
printFpTs = printTypescript (Proxy :: Proxy FpTs)
