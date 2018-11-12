{-# LANGUAGE DeriveAnyClass #-}

module BasicExamples where

import           Internal.Intermediate.Bridge.Generics
import           Internal.Intermediate.Typescript.Generic
import           Data.Text       (Text)
import           GHC.Generics

data SimpleRecord =
  SimpleRecord
    {f1 :: Int
    ,f2 :: Text
    } deriving (Generic, BridgeType, Typescript)

newtype OneFieldRecord =
 OneFieldRecord
    {onlyField :: Int
    } deriving (Generic, BridgeType, Typescript)

data ComplexRecord =
  ComplexRecord
    {anIntField    :: Int
    ,aTextField    :: Text
    ,aUnion        :: SampleUnion
    ,aMaybeType    :: Maybe Text
    ,aSimpleRecord :: SimpleRecord
    } deriving (Generic, BridgeType)

data SimpleUnTagged = F Int Int deriving (Generic, BridgeType)

data SampleUnion = FirstCon Int | SecondCon Text | BoolCon Bool deriving (Generic, BridgeType, Typescript)
