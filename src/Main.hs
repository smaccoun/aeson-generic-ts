{-# LANGUAGE DeriveAnyClass #-}

module Main where


import           Data.Text
import           GenericMappings
import           GHC.Generics
import           Typescript.Generate

data User = User
    {name :: PersonName
    ,age  :: Int
    ,placesLived :: [Text]
    } deriving (Generic, TypescriptType)

data PersonName =
  PersonName
   {firstName     :: Text
   ,middleInitial :: Maybe Text
   ,lastName      :: Text
   } deriving (Generic, TypescriptType)

main :: IO ()
main = do
  putStrLn $ unpack $ genTypescript allTypes
 where
  allTypes =
    [(GenMany (undefined :: User)), (GenMany (undefined :: PersonName))]
