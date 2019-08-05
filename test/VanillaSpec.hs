module VanillaSpec where

import           BasicExamples

import           Data.Proxy
import           Data.Text                                ( Text )

import           Test.Hspec

import           Typescript.Internal.Flavors.Vanilla
import           Typescript.Internal.Intermediate.Generic
import           Typescript.Internal.Output.PrintForeign

spec :: Spec
spec = describe "vanilla_ts" $ do
    it "works for number" $ do
        printVanilla (Proxy :: Proxy Int) `shouldBe` "number"

    it "works for a list of numbers" $ do
        printVanilla (Proxy :: Proxy [Int]) `shouldBe` "Array<number>"

    describe "it works for records" $ do
        it "works for a simple record type" $ do
            printVanilla (Proxy :: Proxy SimpleRecord)
                `shouldBe` "interface SimpleRecord { \n  f1 : number\n  f2 : string\n}"

        it "works for a record with only one field (newtype)" $ do
            printVanilla (Proxy :: Proxy OneFieldRecord)
                `shouldBe` "interface OneFieldRecord { \n  onlyField : number\n}"

        it "works for a complex record" $ do
            printVanilla (Proxy :: Proxy ComplexRecord)
                `shouldBe` "interface ComplexRecord { \n  anIntField : number\n  aTextField : string\n  aUnion : SampleUnion\n  aMaybeType : string | null \n  aSimpleRecord : SimpleRecord\n}"

    describe "it works for various sum types" $ do
        it "works for a simple sum of primitives" $ do
            printVanilla (Proxy :: Proxy SampleUnion)
                `shouldBe` "type SampleUnion = number | string | boolean"

printVanilla :: (TypescriptType a) => Proxy a -> Text
printVanilla = mkTypescriptDeclaration (Proxy :: Proxy Vanilla)
