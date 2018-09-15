{-# LANGUAGE DeriveAnyClass #-}

module Main where

import GenericMappings
import GHC.Generics
import Data.Text
import Typescript.Generate
import Data.Maybe

data User =
  User
    {name         :: Text
    ,age          :: Int
    } deriving (Generic, TypescriptType)

sampleUser :: User
sampleUser = User "Jane Smith" 45

main :: IO ()
main = do
  putStrLn $ fromMaybe "" $ (unpack . toTypescript) <$> toTypescriptType sampleUser
  putStrLn $ show $ toTypescriptType sampleUser
