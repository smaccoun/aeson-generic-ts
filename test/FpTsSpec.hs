{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module FpTsSpec where

import           Data.Proxy
import           Data.Text                                ( Text )

import           GHC.Generics

import           Test.Hspec

import           Typescript.Internal.Flavors.FpTs
import           Typescript.Internal.Intermediate.Generic
import           Typescript.Internal.Output.PrintForeign
import           Typescript.TH.GenInstances

newtype AnOption = AnOption (Maybe Text)
    deriving ( Generic )

newtype F =
  F { f1 :: AnOption }
    deriving ( Generic )

$(deriveTypescriptTypesRecursively [ ''F ])

spec :: Spec
spec = describe "option_type" $ do
    it "handles_a_simple_option" $ do
        let knownSolution = "interface AnOption { \n   : Option<string>\n}"
        printFpTs (Proxy :: Proxy AnOption) `shouldBe` knownSolution

    it "handles_a_simple_option" $ do
        let answer = "interface F { \n  f1 : AnOption\n}"
        printFpTs (Proxy :: Proxy F) `shouldBe` answer

printFpTs :: (TypescriptType a) => Proxy a -> Text
printFpTs = mkTypescriptDeclaration (Proxy :: Proxy FpTs)
