module VanillaSpec where

import           BasicExamples
import           Data.Proxy
import           Data.Text                                (Text)
import           Internal.Intermediate.Typescript.Generic
import           Internal.Output.PrintForeign
import           Internal.Typescript.Flavors.Vanilla
import           Test.Hspec

spec :: Spec
spec = describe "vanilla_ts" $ do
  it "works for number" $ do
    printVanilla (Proxy :: Proxy Int) `shouldBe` "number"

  it "works for number" $ do
    printVanilla (Proxy :: Proxy [Int]) `shouldBe` "Array<number>"

  describe "it works for records" $ do
    it "works for a simple record type" $ do
      printVanilla (Proxy :: Proxy SimpleRecord)
        `shouldBe` "interface SimpleRecord { \n  f1 : number\n  f2 : string\n}"

    it "works for a record with only one field (newtype)" $ do
      printVanilla (Proxy :: Proxy OneFieldRecord)
        `shouldBe` "interface OneFieldRecord { \n  onlyField : number\n}"

  describe "it works for various sum types" $ do
    it "works for a simple sum of primitives" $ do
      printVanilla (Proxy :: Proxy SampleUnion)
        `shouldBe` "type SampleUnion = number | string | boolean"



printVanilla :: (Typescript a) => Proxy a -> Text
printVanilla = mkTypescriptDeclaration (Proxy :: Proxy Vanilla)
