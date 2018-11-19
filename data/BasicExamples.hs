{-# LANGUAGE DeriveAnyClass #-}

module BasicExamples where

import           Data.Text                                           (Text)
import           GHC.Generics
import           Typescript.Internal.Intermediate.Typescript.Generic

data SimpleRecord =
  SimpleRecord
    {f1 :: Int
    ,f2 :: Text
    } deriving (Generic, TypescriptType)

newtype OneFieldRecord =
 OneFieldRecord
    {onlyField :: Int
    } deriving (Generic, TypescriptType)

data ComplexRecord =
  ComplexRecord
    {anIntField    :: Int
    ,aTextField    :: Text
    ,aUnion        :: SampleUnion
    ,aMaybeType    :: Maybe Text
    ,aSimpleRecord :: SimpleRecord
    } deriving (Generic, TypescriptType)

data SampleUnion = FirstCon Int | SecondCon Text | BoolCon Bool deriving (Generic, TypescriptType)
