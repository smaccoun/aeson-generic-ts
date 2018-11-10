module Main (main) where

import System.FilePath.Glob
import Test.DocTest

main :: IO ()
main = do
    let options =
            [ "-XOverloadedStrings"
              ,"-XConstraintKinds"
              ,"-XDataKinds"
              ,"-XDeriveGeneric"
              ,"-XFlexibleContexts"
              ,"-XFlexibleInstances"
              ,"-XKindSignatures"
              ,"-XMultiParamTypeClasses"
              ,"-XOverloadedStrings"
              ,"-XRecordWildCards"
              ,"-XScopedTypeVariables"
              ,"-XStandaloneDeriving"
              ,"-XTypeFamilies"
              ,"-XUndecidableInstances"
            ]

    paths <- globDir1 (compile "**/*.hs") "src"
    doctest $ options ++ paths
