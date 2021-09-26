module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest args where
  extensions =
    [ "LambdaCase"
    , "PatternSynonyms"
    , "BangPatterns"
    , "DerivingStrategies"
    , "DeriveGeneric"
    , "DeriveAnyClass"
    , "GeneralizedNewtypeDeriving"
    , "DeriveTraversable"
    ]
  extArgs = fmap ("-X" ++) extensions
  files =
    [ "Ltlspec"
    ]
  fileArgs = fmap (\f -> "src/" ++ f ++ ".hs") files
  args = ["--fast", "-isrc"] ++ extArgs ++ fileArgs
