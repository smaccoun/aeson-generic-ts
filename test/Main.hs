{-# LANGUAGE DeriveAnyClass #-}

module Main where

import           Spec
import           Test.Hspec

main :: IO ()
main = do
  hspec aesonGenericTSSpec
--  putStrLn $ unpack $ genTypescript allTypes
-- where
--  allTypes =
--    [(GenMany (undefined :: User)), (GenMany (undefined :: PersonName))]
