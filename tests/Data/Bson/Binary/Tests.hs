module Data.Bson.Binary.Tests
    ( tests
    ) where

import Data.Binary.Get(runGet)
import Data.Binary.Put(runPut)
import Data.Bson(Document)
import Data.Bson.Arbitrary()
import Data.Bson.Binary(getDocument,putDocument)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck(Property,(===))

testIdentity :: Document -> Property
testIdentity d = d === runGet getDocument (runPut (putDocument d))

tests :: Test
tests = testGroup "Data.Bson.Binary.Tests"
    [testProperty "id == get . put" testIdentity
    ]
