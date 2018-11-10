{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FpTsSpec where

import           Data.Proxy
import           Data.Text          (Text)
import           Internal.Intermediate.Typescript.Generic
import           GHC.Generics
import           Internal.Output.PrintForeign
import           Test.Hspec
import           Internal.Typescript.Flavors.FpTs

spec :: Spec
spec = describe "option_type" $ do
  it "handles_a_simple_option" $ do
    printFpTs (Proxy :: Proxy AnOption) `shouldBe` knownSolution
  where knownSolution = "Option<string>"


newtype AnOption = AnOption (Maybe Text) deriving (Generic, Typescript)

printFpTs :: (Typescript a) => Proxy a -> Text
printFpTs = mkTypescriptDeclaration (Proxy :: Proxy FpTs)
