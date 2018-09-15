{-# LANGUAGE DeriveAnyClass #-}

module Main where

import GenericMappings
import GHC.Generics
import Data.Text

data SampleType = AConstructor Int deriving (Generic, TypescriptType)

data ARecord =
  ARecord
    {firstField :: Int
    ,aStringField :: Text
    } deriving (Generic, TypescriptType)

main :: IO ()
main = do
  putStrLn $ show $ toTypescriptType (3 :: Int)
  putStrLn $ show $ toTypescriptType (AConstructor 3)
  putStrLn $ show $ toTypescriptType (ARecord 3 "Meow")
  putStrLn "hello world"
