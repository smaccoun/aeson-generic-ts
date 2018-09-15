{-# LANGUAGE DeriveAnyClass #-}

module Main where

import GenericMappings
import GHC.Generics
import Data.Text
import Data.Aeson
import Typescript.Generate
import Data.Maybe

data SampleType = AConstructor Int Text deriving (Generic, TypescriptType, ToJSON)

sample :: SampleType
sample = AConstructor 3 "Meow"

data ARecord =
  ARecord
    {firstField :: Int
    ,aStringField :: Text
    } deriving (Generic, TypescriptType)

main :: IO ()
main = do
  putStrLn $ fromMaybe "" $ (unpack . toTypescript) <$> toTypescriptType (ARecord 3 "Meow")
