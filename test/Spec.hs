{-# LANGUAGE DeriveAnyClass #-}

module Spec where

import           Bridge.Generics
import           Data.Proxy
import           Data.Text           (Text)
import qualified Data.Text           as T
import           GHC.Generics
import           PrintForeign
import           Test.Hspec

data SimpleRecord =
  SimpleRecord
    {f1 :: Int
    } deriving (Generic, BridgeType)

data ComplexRecord =
  ComplexRecord
    {anIntField :: Int
    ,aTextField :: Text
    ,aUnion     :: SampleUnion
    ,aMaybeType :: Maybe Text
    ,aSimpleRecord :: SimpleRecord
    } deriving (Generic, BridgeType)

data SimpleUnTagged = F Int deriving (Generic, BridgeType)

data SampleUnion = FirstCon Int | SecondCon Text deriving (Generic, BridgeType)

aesonGenericTSSpec :: Spec
aesonGenericTSSpec = do
  describe "translates_to_all_primitives" $ do
    it "works for number" $ do
      t <- printFromBridge (Proxy :: Proxy Int)
      t `shouldBe` "number"

    it "works for number" $ do
      t <- printFromBridge (Proxy :: Proxy [Int])
      t `shouldBe` "Array<number>"

    it "Should output the correct complex record" $ do
      ts <- printFromBridge (Proxy :: Proxy ComplexRecord)
      ts `shouldBe` knownSolution
      where
        knownSolution =
          T.intercalate ""
            ["interface ComplexRecord { \n"
            ,"anIntField : number\n"
            ,"aTextField : string\n"
            ,"aUnion : SampleUnion\n"
            ,"aMaybeType : string | null \n"
            ,"aSimpleRecord : SimpleRecord}"
            ]
