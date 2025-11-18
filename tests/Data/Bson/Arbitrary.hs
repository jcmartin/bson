{-# LANGUAGE FlexibleInstances, OverloadedStrings, TypeSynonymInstances #-}
module Data.Bson.Arbitrary() where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Data.Int(Int64)
import Data.Time.Clock.POSIX (POSIXTime,posixSecondsToUTCTime)
import Data.Time.Clock (UTCTime(..))
import qualified Data.ByteString as S

import Data.Text (Text)
import Test.QuickCheck (Arbitrary(..), elements, oneof,sized,resize)
import Test.QuickCheck.Modifiers(getPrintableString)
import qualified Data.Text as T

import Data.Bson (ObjectId(..), MinMaxKey(..), MongoStamp(..),
                  Symbol(..), Javascript(..), Regex(..), UserDefined(..),
                  MD5(..), UUID(..), Function(..), Binary(..), Field((:=)),
                  Value(..))
import qualified Data.Bson as Bson

instance Arbitrary S.ByteString where
    arbitrary = S.pack <$> arbitrary

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary POSIXTime where
    arbitrary = fromInteger <$> arbitrary

instance Arbitrary UTCTime where
    arbitrary = do
        w <- arbitrary
        return $! posixSecondsToUTCTime $ fromIntegral (w :: Int64) / 1000

instance Arbitrary ObjectId where
    arbitrary = Oid <$> arbitrary <*> arbitrary

instance Arbitrary MinMaxKey where
    arbitrary = elements [MinKey, MaxKey]

instance Arbitrary MongoStamp where
    arbitrary = MongoStamp <$> arbitrary

instance Arbitrary Symbol where
    arbitrary = Symbol <$> arbitrary

instance Arbitrary Javascript where
    arbitrary = Javascript <$> arbitrary <*> arbitrary

instance Arbitrary Regex where
    arbitrary = do
        a <- T.pack . getPrintableString <$> arbitrary
        b <- T.pack . getPrintableString <$> arbitrary
        return $! Regex a b

instance Arbitrary UserDefined where
    arbitrary = do
        s <- arbitrary
        let arbSubType = 128 + s `mod` 128
        UserDefined arbSubType <$> arbitrary

instance Arbitrary MD5 where
    arbitrary = MD5 <$> arbitrary

instance Arbitrary UUID where
    arbitrary = UUID <$> arbitrary

instance Arbitrary Function where
    arbitrary = Function <$> arbitrary

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary

instance Arbitrary Field where
    arbitrary = do
        fieldName <- T.pack . getPrintableString <$> arbitrary
        (fieldName :=) <$> arbitrary

instance Arbitrary Value where
    arbitrary = sized $ \n ->
        if n <= 1
            then oneof
                [ Bson.Float   <$> arbitrary
                , Bson.String  <$> arbitrary
                --, Bson.Doc     <$> arbitrary
                --, Bson.Array   <$> arbitrary
                , Bson.Bin     <$> arbitrary
                , Bson.Fun     <$> arbitrary
                , Bson.Uuid    <$> arbitrary
                , Bson.Md5     <$> arbitrary
                , Bson.UserDef <$> arbitrary
                , Bson.ObjId   <$> arbitrary
                , Bson.UTC     <$> arbitrary
                , Bson.RegEx   <$> arbitrary
                --, Bson.JavaScr <$> arbitrary
                , Bson.Sym     <$> arbitrary
                , Bson.Int32   <$> arbitrary
                , Bson.Int64   <$> arbitrary
                , Bson.Stamp   <$> arbitrary
                , Bson.MinMax  <$> arbitrary
                , return Bson.Null
                ]
            else oneof
                [ Bson.Float   <$> arbitrary
                , Bson.String  <$> arbitrary
                , Bson.Doc     <$> resize (n `div` 2) arbitrary
                , Bson.Array   <$> resize (n `div` 2) arbitrary
                , Bson.Bin     <$> arbitrary
                , Bson.Fun     <$> arbitrary
                , Bson.Uuid    <$> arbitrary
                , Bson.Md5     <$> arbitrary
                , Bson.UserDef <$> arbitrary
                , Bson.ObjId   <$> arbitrary
                , Bson.UTC     <$> arbitrary
                , Bson.RegEx   <$> arbitrary
                , Bson.JavaScr <$> resize (n `div` 2) arbitrary
                , Bson.Sym     <$> arbitrary
                , Bson.Int32   <$> arbitrary
                , Bson.Int64   <$> arbitrary
                , Bson.Stamp   <$> arbitrary
                , Bson.MinMax  <$> arbitrary
                , return Bson.Null
                ]
