{-# LANGUAGE DeriveAnyClass #-}

module Spec where

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

