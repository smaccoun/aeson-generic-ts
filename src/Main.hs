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

main :: IO ()
main = do
  putStrLn $ unpack $ printTS (Proxy :: Proxy User)
  putStrLn $ unpack $ printTS (Proxy :: Proxy PersonName)
  putStrLn $ unpack $ genTypescript allTypes
  putStrLn $ show $ toTypescriptType (Proxy :: Proxy User)
  where
    allTypes =
      [(GenMany (undefined :: User))
      ,(GenMany (undefined :: PersonName))
      ]
