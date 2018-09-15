{-# LANGUAGE DeriveAnyClass #-}

module Main where

import GenericMappings
import GHC.Generics
import Data.Text
import Typescript.Generate
import Data.Maybe

data ARecord =
  ARecord
    {firstField :: Int
    ,aStringField :: Text
    } deriving (Generic, TypescriptType)

sample :: ARecord
sample = ARecord 3 "Meow"

main :: IO ()
main = do
  putStrLn $ fromMaybe "" $ (unpack . toTypescript) <$> toTypescriptType sample
  putStrLn $ show $ toTypescriptType sample
