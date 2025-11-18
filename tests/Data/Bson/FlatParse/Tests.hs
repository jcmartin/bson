module Data.Bson.FlatParse.Tests
    ( tests
    ) where

import Data.Binary.Put(runPut)
import Data.Bson(Document)
import Data.Bson.Arbitrary()
import Data.Bson.Binary(putDocument)
import Data.Bson.FlatParse(parseDocument)
import qualified Data.ByteString as BS
import FlatParse.Basic(runParser,Result(..))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck(Property,(===))

testIdentity :: Document -> Property
testIdentity d = case runParser parseDocument $ BS.toStrict (runPut (putDocument d)) of
    OK a _ -> d === a
    Fail   -> error "Decode error"
    Err e  -> error e

tests :: Test
tests = testGroup "Data.Bson.FlatParse.Tests"
    [testProperty "id == parse . put" testIdentity
    ]

