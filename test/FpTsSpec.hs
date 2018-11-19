{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FpTsSpec where

import           Data.Proxy
import           Data.Text                                (Text)
import           GHC.Generics
import           Test.Hspec
import           Typescript.Internal.Flavors.FpTs
import           Typescript.Internal.Intermediate.Generic
import           Typescript.Internal.Output.PrintForeign

spec :: Spec
spec = describe "option_type" $ do
  it "handles_a_simple_option" $ do
    printFpTs (Proxy :: Proxy AnOption) `shouldBe` knownSolution
  where knownSolution = "Option<string>"


newtype AnOption = AnOption (Maybe Text) deriving (Generic, TypescriptType)

printFpTs :: (TypescriptType a) => Proxy a -> Text
printFpTs = mkTypescriptDeclaration (Proxy :: Proxy FpTs)
