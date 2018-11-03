module VanillaSpec where

import           BasicExamples
import           Data.Proxy
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Internal.Intermediate.Bridge.Generics
import           Internal.Output.PrintForeign
import           Internal.Typescript.Flavors.Vanilla
import           Test.Hspec

printVanilla :: (BridgeType a) => Proxy a -> IO Text
printVanilla = printTypescript (Proxy :: Proxy Vanilla)

spec :: Spec
spec = describe "vanilla_ts" $ do
  it "works for number" $ do
    t <- printVanilla (Proxy :: Proxy Int)
    t `shouldBe` "number"

  it "works for number" $ do
    t <- printVanilla (Proxy :: Proxy [Int])
    t `shouldBe` "Array<number>"

  it "Should output the correct complex record" $ do
    putStrLn $ T.unpack knownSolution
    ts <- printVanilla (Proxy :: Proxy ComplexRecord)
    putStrLn $ T.unpack ts
    ts `shouldBe` knownSolution
 where
  knownSolution = T.intercalate
    ""
    [ "interface ComplexRecord { \n"
    , "  anIntField : number\n"
    , "  aTextField : string\n"
    , "  aUnion : SampleUnion\n"
    , "  aMaybeType : string | null \n"
    , "  aSimpleRecord : SimpleRecord"
    , "\n}"
    ]
