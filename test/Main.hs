{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           Data.Text
import           GenericMappings
import           GHC.Generics
import           Internal.Generate
import           Spec
import           Test.Hspec

data User = User
    { name         :: PersonName
    , age          :: Int
    , placesLived  :: [Text]
    , isRegistered :: Bool
    } deriving (Generic, TypescriptType)

data PersonName =
  PersonName
   { firstName     :: Text
   , middleInitial :: Maybe Text
   , lastName      :: Text
   } deriving (Generic, TypescriptType)


main :: IO ()
main = do
  putStrLn $ unpack $ genTypescript allTypes
  hspec aesonGenericTSSpec
 where
  allTypes =
    [(GenMany (undefined :: User)), (GenMany (undefined :: PersonName))]
