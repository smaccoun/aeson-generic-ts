{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FpTsSpec where

import           Data.Proxy
import           Data.Text                                (Text)
import           GHC.Generics
import           Internal.Intermediate.Typescript.Generic
import           Internal.Output.PrintForeign
import           Internal.Typescript.Flavors.FpTs
import           Test.Hspec

spec :: Spec
spec = describe "option_type" $ do
  it "handles_a_simple_option" $ do
    printFpTs (Proxy :: Proxy AnOption) `shouldBe` knownSolution
  where knownSolution = "Option<string>"


newtype AnOption = AnOption (Maybe Text) deriving (Generic, TypescriptType)

printFpTs :: (TypescriptType a) => Proxy a -> Text
printFpTs = mkTypescriptDeclaration (Proxy :: Proxy FpTs)
