{-# LANGUAGE DeriveAnyClass #-}

module Spec where

import           Bridge.Generics
import           Data.Proxy
import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Generics
import           PrintForeign
import           Test.Hspec
import           Typescript.Vanilla
import           FpTsSpec

data SimpleRecord =
  SimpleRecord
    {f1 :: Int
    } deriving (Generic, BridgeType)

data ComplexRecord =
  ComplexRecord
    {anIntField    :: Int
    ,aTextField    :: Text
    ,aUnion        :: SampleUnion
    ,aMaybeType    :: Maybe Text
    ,aSimpleRecord :: SimpleRecord
    } deriving (Generic, BridgeType)

data SimpleUnTagged = F Int deriving (Generic, BridgeType)

data SampleUnion = FirstCon Int | SecondCon Text deriving (Generic, BridgeType)

aesonGenericTSSpec :: Spec
aesonGenericTSSpec = do
  FpTsSpec.spec

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

