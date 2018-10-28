module VanillaSpec where

import           BasicExamples
import           Data.Proxy
import qualified Data.Text                             as T
import           Internal.Output.PrintForeign
import           Internal.Typescript.Flavors.Vanilla
import           Test.Hspec


spec :: Spec
spec = do
  describe "vanilla_ts" $ do
    let vanilla = (Proxy :: Proxy Vanilla)
    it "works for number" $ do
      t <- printFromBridge vanilla (Proxy :: Proxy Int)
      t `shouldBe` "number"

    it "works for number" $ do
      t <- printFromBridge vanilla (Proxy :: Proxy [Int])
      t `shouldBe` "Array<number>"

    it "Should output the correct complex record" $ do
      putStrLn $ T.unpack knownSolution
      ts <- printFromBridge vanilla (Proxy :: Proxy ComplexRecord)
      putStrLn $ T.unpack ts
      ts `shouldBe` knownSolution
      where
        knownSolution =
          T.intercalate ""
            ["interface ComplexRecord { \n"
            ,"  anIntField : number\n"
            ,"  aTextField : string\n"
            ,"  aUnion : SampleUnion\n"
            ,"  aMaybeType : string | null \n"
            ,"  aSimpleRecord : SimpleRecord"
            ,"\n}"
            ]
