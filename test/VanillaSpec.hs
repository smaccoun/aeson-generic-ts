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

  describe "it works for various sum types" $ do
    it "works for a simple sum of primitives" $ do
      printVanilla (Proxy :: Proxy SampleUnion)
        `shouldBe` "type SampleUnion = number | string | boolean"


printVanilla :: (Typescript a) => Proxy a -> Text
printVanilla = mkTypescriptDeclaration (Proxy :: Proxy Vanilla)
