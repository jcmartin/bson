{-# LANGUAGE FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}
module Data.Bson.Tests
    ( tests
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
#if !MIN_VERSION_base(4,9,0)
import Control.Monad.Fail(MonadFail(..))
#endif
import Data.Int (Int32, Int64)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.Clock (UTCTime)

import Data.Text (Text)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Property, (===))

import Data.Bson (Val(cast', val), ObjectId(..), MinMaxKey(..), MongoStamp(..),
                  Symbol(..), Javascript(..), Regex(..), UserDefined(..),
                  MD5(..), UUID(..), Function(..), Binary(..),
                  Document,
                  Value(..))
import qualified Data.Bson as Bson
import Data.Bson.Arbitrary()


testVal :: Val a => a -> Property
testVal a = case cast' . val $ a of
    Nothing -> error "Cast failed"
    Just a' -> a === a'

#if MIN_VERSION_base(4,9,0)
instance MonadFail (Either String) where
   fail = Left

testLookMonadFail :: Property
testLookMonadFail =
   (Bson.look "key" [] :: Either String Value)
      -- This is as opposed to an exception thrown from Prelude.fail:
      === Left "expected \"key\" in []"
#endif

tests :: Test
tests = testGroup "Data.Bson.Tests"
    [ testProperty "Val Bool"        (testVal :: Bool -> Property)
    , testProperty "Val Double"      (testVal :: Double -> Property)
    , testProperty "Val Float"       (testVal :: Float -> Property)
    , testProperty "Val Int"         (testVal :: Int -> Property)
    , testProperty "Val Int32"       (testVal :: Int32 -> Property)
    , testProperty "Val Int64"       (testVal :: Int64 -> Property)
    , testProperty "Val Integer"     (testVal :: Integer -> Property)
    , testProperty "Val String"      (testVal :: String -> Property)
    , testProperty "Val POSIXTime"   (testVal :: POSIXTime -> Property)
    , testProperty "Val UTCTime"     (testVal :: UTCTime -> Property)
    , testProperty "Val ObjectId"    (testVal :: ObjectId -> Property)
    , testProperty "Val MinMaxKey"   (testVal :: MinMaxKey -> Property)
    , testProperty "Val MongoStamp"  (testVal :: MongoStamp -> Property)
    , testProperty "Val Symbol"      (testVal :: Symbol -> Property)
    , testProperty "Val Javascript"  (testVal :: Javascript -> Property)
    , testProperty "Val Regex"       (testVal :: Regex -> Property)
    , testProperty "Val UserDefined" (testVal :: UserDefined -> Property)
    , testProperty "Val MD5"         (testVal :: MD5 -> Property)
    , testProperty "Val UUID"        (testVal :: UUID -> Property)
    , testProperty "Val Function"    (testVal :: Function -> Property)
    , testProperty "Val Binary"      (testVal :: Binary -> Property)
    , testProperty "Val Document"    (testVal :: Document -> Property)
    , testProperty "Val Text"        (testVal :: Text -> Property)

#if MIN_VERSION_base(4,9,0)
    , testProperty "'look' uses MonadFail.fail" testLookMonadFail
#endif
    ]
