{-# LANGUAGE DeriveAnyClass #-}

module Spec where

import           Bridge.Generics
import           BridgeToTypescript
import           Data.Text
import           GenericMappings
import           GHC.Generics
import           Test.Hspec
import           Typescript.Types

data ARecord =
  ARecord
    {firstField   :: Int
    ,aStringField :: Text
    } deriving (Generic, TypescriptType)

sample :: ARecord
sample = ARecord 3 "Meow"

data ComplexRecord =
  ComplexRecord
    {anIntField   :: Int
    ,aTextField   :: Text
    ,aUnion       :: SampleUnion
    } deriving (Generic, BridgeType)

complexExample :: ComplexRecord
complexExample = ComplexRecord 3 "meow" (FirstCon 3)

data SimpleUnTagged = F Int deriving (Generic, BridgeType)

data SampleUnion = FirstCon Int | SecondCon Text deriving (Generic, BridgeType)

aesonGenericTSSpec :: Spec
aesonGenericTSSpec = do
  describe "Record to interface " $ do
    it "Should match the given TStype"
      $          toTypescriptType sample
      `shouldBe` (Just $ TSInterface
                   "ARecord"
                   [ TSField (FieldName "firstField") (TSPrimitiveType TSNumber)
                   , TSField (FieldName "aStringField")
                             (TSPrimitiveType TSString)
                   ]
                 )
    it "Should match the given TStype from Bridge" $
      (bridgeTypeToTSType $ toBridgeType complexExample)
      `shouldBe` (TSInterface
                   "ComplexRecord"
                   [ TSField (FieldName "anIntField") (TSPrimitiveType TSNumber)
                   , TSField (FieldName "aTextField") (TSPrimitiveType TSString)
                   ]
                 )


