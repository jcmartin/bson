{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Main(main) where

import Control.DeepSeq
import Criterion.Main
import Data.Binary.Get(runGet)
import Data.Binary.Put(runPut)
import Data.Bson
import Data.Bson.Binary(getDocument,putDocument)
import Data.Bson.FlatParse(parseDocument)
import qualified Data.ByteString as BS
import FlatParse.Basic(runParser,Result(..))

instance NFData Field
instance NFData Value
instance NFData Binary
instance NFData UUID
instance NFData Function
instance NFData MD5
instance NFData Encrypted
instance NFData Compressed
instance NFData Sensitive
instance NFData Vector
instance NFData UserDefined
instance NFData Regex
instance NFData ObjectId
instance NFData Javascript
instance NFData Symbol
instance NFData MongoStamp
instance NFData Decimal128
instance NFData MinMaxKey

complexDoc1 :: Document
complexDoc1 =
    ["hello" := Int64 82
    ,"asdfsdafhiuadflwqef;asdf" := Doc
        ["SDf1DD" := Doc
            ["SDFDF" := Doc
                ["1///13u3" := Doc
                    ["POI" := Float 2.48271
                    ]
                ,"1373rjk" := Array [Float 2.248,String "HIYADFJHD",Bool True]
                ]
            ,"&DFJDKDLD" := String "y718uhilewfihluawefhiluafwlhiuafwhiluaewfuwefaliuwef8u13kjasd.knzxv"
            ]
        ,"1kasdfkjadsj;asdhjdsasdfasdfsa" := Bin (Binary "SDFF>/191r3khzxcv;iu13rhiuasdflk;adsflkh")
        ]
    ,"3292ihuvn.zxcv;89" := Array [Float 214124,Float 3e17,Int64 2321]
    ]

onlyNumbers1 :: Document
onlyNumbers1 =
    ["hello" := Int64 82
    ,"POI" := Float 2.48271
    ,"1373rjk" := Float 2.248
    ,"3292ihuvn.zxcv;89" := Float 214124
    ,"SDF*" := Float 3e17
    ,"SDF*FF" := Int64 2321
    ]

onlyStrings1 :: Document
onlyStrings1 =
    ["&DFJDKDLD" := String "y718uhilewfihluawefhiluafwlhiuafwhiluaewfuwefaliuwef8u13kjasd.knzxv"
    ,"SDF&UK" := String "1ruih;ladsfihlur23384t3u8348ualuhiafiulasdfluasdf.zxcv;hoisda"
    ,"2p;22thsdfdsadsfsadf" := String "1;ug1r2lr1./ads/fasdfpazpppp13iruo12lkjds;afdhjkasdjf"
    ,"2sadsfsadf" := String "1;uasdfs7adfsap9;iklj/.lkjpzxcvg1r2lr1./ads/fasdfpazpppp13iruo12lkjds;afdhjkasdjf"
    ,"f" := String "1;uasdfs7adfsap9;iklj/.lkjpzxcvg1r2lr1./ads/fasdfpazpppp13iruo12lkjds;afdhjkasdjf1771"
    ]

onlyFlatArrays :: Document
onlyFlatArrays =
    ["1373rjk" := Array [Float 2.248,String "HIYADFJHD",Bool True]
    ,"3292ihuvn.zxcv;89" := Array [Float 214124,Float 3e17,Int64 2321]
    ,".zxcv;89" := Array [String "DF*DSDF",String "SDF&USDF", Bool True,Int64 214124,Float 3e17,Int64 2321]
    ]

onlyDocNull :: Document
onlyDocNull =
    ["asdfsdafhiuadflwqef;asdf" := Doc
        ["SDf1DD" := Doc
            ["SDFDF" := Doc
                ["1///13u3" := Doc
                    ["POI" := Null
                    ]
                ]
            ]
        ]
    ]

benchBinary :: (forall a b . NFData b => (a -> b) -> a -> Benchmarkable) -> BS.ByteString -> Benchmark
benchBinary f =
    bench "Binary" . f (runGet getDocument) . BS.fromStrict

benchFlatParse :: (forall a b . NFData b => (a -> b) -> a -> Benchmarkable) ->  BS.ByteString -> Benchmark
benchFlatParse f =
    bench "Flat" . f (fromResult . runParser parseDocument)

fromResult :: Result e a -> a
fromResult x = case x of
    OK a _ -> a
    _      -> error "Should be unreachable"

benchParsers :: String -> Document -> Benchmark
benchParsers name doc =
    let input = BS.toStrict $ runPut $ putDocument doc
    in bgroup name
        [bgroup "Normal Form"
            [benchBinary nf input
            ,benchFlatParse nf input
            ]
        ,bgroup "WHNF"
            [benchBinary whnf input
            ,benchFlatParse whnf input
            ]
        ]

main :: IO ()
main = defaultMain
    [benchParsers "onlyDocNull x50" (concat (replicate 50 onlyDocNull))
    ,benchParsers "onlyFlatArrays x50" (concat (replicate 50 onlyFlatArrays))
    ,benchParsers "onlyStrings1 x50" (concat (replicate 50 onlyStrings1))
    ,benchParsers "onlyNumbers1 x50" (concat (replicate 50 onlyNumbers1))
    ,benchParsers "complexDoc1 x50" (concat (replicate 50 complexDoc1))
    ,benchParsers "Empty" []
    ,benchParsers "SingleFieldInt64" ["hello" := Int64 23]
    ]
