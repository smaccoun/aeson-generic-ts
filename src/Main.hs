{-# LANGUAGE DeriveAnyClass #-}

module Main where


import           Data.Proxy
import           Data.Text
import           GenericMappings
import           GHC.Generics
import           Typescript.Generate

data User = User
    {name :: PersonName
    ,age  :: Int
    } deriving (Generic, TypescriptType)

data PersonName =
  PersonName
   {firstName     :: Text
   ,middleInitial :: Maybe Text
   ,lastName      :: Text
   } deriving (Generic, TypescriptType)

sampleUser :: User
sampleUser = User (PersonName "Jane" Nothing "Smithg") 45

main :: IO ()
main = do
  putStrLn $ unpack $ printTS (Proxy :: Proxy User)
  putStrLn $ unpack $ printTS (Proxy :: Proxy PersonName)
  putStrLn $ show $ toTypescriptType sampleUser
