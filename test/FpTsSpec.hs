{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}

module FpTsSpec where

import           Data.Proxy
import           Data.Text                                (Text)
import           GHC.Generics
import           Test.Hspec
import           Typescript.Internal.Flavors.FpTs
import           Typescript.Internal.Intermediate.Generic
import           Typescript.Internal.Output.PrintForeign

newtype AnOption = AnOption (Maybe Text) deriving (Generic, TypescriptType)

spec :: Spec
spec = describe "option_type" $ do
  it "handles_a_simple_option" $ do
    printFpTs (Proxy :: Proxy AnOption) `shouldBe` knownSolution
  where knownSolution = "Option<string>"



printFpTs :: (TypescriptType a) => Proxy a -> Text
printFpTs = mkTypescriptDeclaration (Proxy :: Proxy FpTs)
