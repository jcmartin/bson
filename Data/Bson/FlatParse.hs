module Data.Bson.FlatParse(parseDocument) where

import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word8)

import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import Data.Bson (Document, Value(..), ObjectId(..), MongoStamp(..), Symbol(..),
                  Javascript(..), UserDefined(..), Regex(..), MinMaxKey(..),
                  Binary(..), UUID(..), Field(..), MD5(..), Function(..),
                  Encrypted(..), Compressed(..), Sensitive(..), Vector(..),
                  Decimal128(..)
                  )

import FlatParse.Basic
import qualified FlatParse.Basic as FlatParse
import GHC.Float(castWord64ToDouble)

parseDocument :: ParserT st String Document
parseDocument = do
  len <- subtract 4 <$> anyInt32le
  curPos@(Pos p) <- getPos
  let newPos = Pos $ p - fromIntegral len
  r <- inSpan (Span curPos newPos) parseFields
  setPos newPos
  return r

parseFields :: ParserT st String [Field]
parseFields = do
    tag <- parseTag
    if tag == 0
        then return []
        else (:) <$> parseField tag <*> parseFields

parseField :: Word8 -> ParserT st String Field
-- ^ Read binary representation of Element
parseField t = do
  k <- parseLabel
  v <- case t of
        0x01 -> Float <$> parseDouble
        0x02 -> String <$> parseString
        0x03 -> Doc <$> parseDocument
        0x04 -> Array <$> parseArray
        0x05 -> parseBinary >>= \(s, b) ->
          case s of
           0x00 -> return $ Bin (Binary b)
           0x01 -> return $ Fun (Function b)
           0x02 -> return $ Bin (Binary b)
           0x03 -> return $ Uuid (UUID b)
           0x04 -> return $ Uuid (UUID b)
           0x05 -> return $ Md5 (MD5 b)
           0x06 -> return $ Encrypt (Encrypted b)
           0x07 -> return $ Compress (Compressed b)
           0x08 -> return $ Sens (Sensitive b)
           0x09 -> return $ Vec (Vector b)
           _ ->
            if s >= 0x80
                then return $ UserDef (UserDefined s b)
                else err $ "unknown Bson binary subtype " ++ show s
        0x06 -> return Null
        0x07 -> ObjId <$> parseObjectId
        0x08 -> Bool <$> parseBool
        0x09 -> UTC <$> parseUTC
        0x0A -> return Null
        0x0B -> RegEx <$> parseRegex
        0x0C -> ObjId <$> parseObjectId <* parseString
        0x0D -> JavaScr . Javascript [] <$> parseString
        0x0E -> Sym <$> parseSymbol
        0x0F -> JavaScr . uncurry (flip Javascript) <$> parseClosure
        0x10 -> Int32 <$> anyInt32le
        0x11 -> Stamp <$> parseMongoStamp
        0x12 -> Int64 <$> anyInt64le
        0x13 -> Dec128 <$> parseDec128
        0xFF -> return (MinMax MinKey)
        0x7F -> return (MinMax MaxKey)
        _ -> err $ "unknown Bson value type " ++ show t
  return (k := v)

parseTag :: ParserT st e Word8
parseTag = anyWord8

parseLabel :: ParserT st e Text
parseLabel = parseCString

parseCString :: ParserT st e Text
parseCString = TE.decodeUtf8 <$> anyCString

parseString :: ParserT st e Text
parseString = do
  len <- subtract 1 <$> anyInt32le
  b <- FlatParse.take (fromIntegral len)
  _ <- anyWord8
  return $! TE.decodeUtf8 b

parseMongoStamp :: ParserT st e MongoStamp
parseMongoStamp = MongoStamp <$> anyInt64le

parseObjectId :: ParserT st e ObjectId
parseObjectId = Oid <$> anyWord32be <*> anyWord64be

type Subtype = Word8

parseBinary :: ParserT st e (Subtype, ByteString)
parseBinary = do
  len <- anyInt32le
  t <- parseTag
  x <- FlatParse.take (fromIntegral len)
  return (t, x)

parseDec128 :: ParserT st e Decimal128
parseDec128 = Decimal128 <$> FlatParse.take 16

parseClosure :: ParserT st String (Text, Document)
parseClosure = do
  _ <- anyInt32le
  x <- parseString
  y <- parseDocument
  return (x, y)

parseArray :: ParserT st String [Value]
parseArray = map value <$> parseDocument

parseBool :: ParserT st e Bool
parseBool = (> 0) <$> anyWord8

parseSymbol :: ParserT st e Symbol
parseSymbol = Symbol <$> parseString

parseRegex :: ParserT st e Regex
parseRegex = Regex <$> parseCString <*> parseCString

parseUTC :: ParserT st e UTCTime
-- stored as milliseconds since Unix epoch
parseUTC = posixSecondsToUTCTime . (/ 1000) . fromIntegral <$> anyInt64le

parseDouble :: ParserT st e Double
parseDouble = castWord64ToDouble <$> anyWord64le
