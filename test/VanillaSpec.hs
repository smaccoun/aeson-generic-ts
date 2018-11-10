module VanillaSpec where

import           Data.Proxy
import           Internal.Intermediate.Typescript.Generic
import           Data.Text                             (Text)
import           Internal.Output.PrintForeign
import           Internal.Typescript.Flavors.Vanilla
import           Test.Hspec


spec :: Spec
spec = describe "vanilla_ts" $ do
  it "works for number" $ do
    printVanilla (Proxy :: Proxy Int) `shouldBe` "number"

  it "works for number" $ do
    printVanilla (Proxy :: Proxy [Int]) `shouldBe` "Array<number>"


printVanilla :: (Typescript a) => Proxy a -> Text
printVanilla = mkTypescriptDeclaration (Proxy :: Proxy Vanilla)
