{-# LANGUAGE DeriveAnyClass #-}

module BasicExamples where

import           Internal.Intermediate.Typescript.Generic
import           Data.Text       (Text)
import           GHC.Generics

data SimpleRecord =
  SimpleRecord
    {f1 :: Int
    ,f2 :: Text
    } deriving (Generic, Typescript)

newtype OneFieldRecord =
 OneFieldRecord
    {onlyField :: Int
    } deriving (Generic, Typescript)

data ComplexRecord =
  ComplexRecord
    {anIntField    :: Int
    ,aTextField    :: Text
    ,aUnion        :: SampleUnion
    ,aMaybeType    :: Maybe Text
    ,aSimpleRecord :: SimpleRecord
    } deriving (Generic, Typescript)

data SampleUnion = FirstCon Int | SecondCon Text | BoolCon Bool deriving (Generic, Typescript)
