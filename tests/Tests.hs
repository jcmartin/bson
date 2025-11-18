module Main where

import Test.Framework (defaultMain)

import qualified Data.Bson.Tests
import qualified Data.Bson.Binary.Tests
import qualified Data.Bson.FlatParse.Tests

main :: IO ()
main = defaultMain
    [ Data.Bson.Tests.tests
    , Data.Bson.Binary.Tests.tests
    , Data.Bson.FlatParse.Tests.tests
    ]
