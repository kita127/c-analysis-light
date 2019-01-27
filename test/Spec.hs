{-# LANGUAGE OverloadedStrings #-}
import           Data.Attoparsec.Text      hiding (take)
import           Data.List                 (intercalate)
import qualified Data.Text                 as T
import           Language.C.Analysis.Light
import           Test.HUnit

exRes :: Result a -> Either String a
exRes (Done _ r)    = Right r
exRes (Fail _ ss s) = Left $ intercalate " : " (s:ss)
exRes (Partial _)   = Left "Partial"

main :: IO ()
main = do
    runTestTT $ TestList
      [ testSample
      , testToken
      , testValue
      , testDefVariable
      , testIdentifire
      ]
    return ()


testSample :: Test
testSample = TestList
  [ "testSample test 1" ~:
        "hello test" ~?= "hello test"
  ]

testToken :: Test
testToken = TestList
  [ "testToken normal 1" ~:
        (exRes $ parse (token identifire) "    hoge_var    " `feed` "") ~?= Right "hoge_var"
  , "testToken normal 2" ~:
        (exRes $ parse (token identifire) "    hoge_var\n    " `feed` "") ~?= Right "hoge_var"
  ]

testDefVariable :: Test
testDefVariable = TestList
  [ "testDefVariable normal 1" ~:
        (exRes $ parse defVariable "int hoge;" `feed` "") ~?= Right
            Var {
              typ = "int"
            , name = "hoge"
            , initVal = Nothing
            }
  , "testDefVariable normal 2" ~:
        (exRes $ parse defVariable "MyStruct st_var;" `feed` "") ~?= Right
            Var {
              typ = "MyStruct"
            , name = "st_var"
            , initVal = Nothing
            }

  , "testDefVariable initial value 1" ~:
        (exRes $ parse defVariable "Hoge yyy_abc = 100;" `feed` "") ~?= Right
            Var {
              typ = "Hoge"
            , name = "yyy_abc"
            , initVal = Just "100"
            }
  , "testDefVariable initial value 2" ~:
        (exRes $ parse defVariable "Hoge     yyy_abc            =    100       ;" `feed` "") ~?= Right
            Var {
              typ = "Hoge"
            , name = "yyy_abc"
            , initVal = Just "100"
            }
  , "testDefVariable initial value 3" ~:
        (exRes $ parse defVariable "char    foobar_xyz   =  VALUE;" `feed` "") ~?= Right
            Var {
              typ = "char"
            , name = "foobar_xyz"
            , initVal = Just "VALUE"
            }
  ]

testIdentifire :: Test
testIdentifire = TestList
  [ "testIdentifire normal 1" ~:
        (exRes $ parse identifire "hoge_var" `feed` "") ~?= Right "hoge_var"
  , "testIdentifire include number 1" ~:
        (exRes $ parse identifire "bar123hoge" `feed` "") ~?= Right "bar123hoge"
  , "testIdentifire first letter which is number 1" ~:
        (exRes $ parse identifire "999_error" `feed` "") ~?= Left "Failed reading: satisfy : '_'"
  ]

testValue :: Test
testValue = TestList
  [ "testValue normal 1" ~:
        (exRes $ parse value "VALUE" `feed` "") ~?= Right "VALUE"
  , "testValue normal 2" ~:
        (exRes $ parse value "234" `feed` "") ~?= Right "234"
  , "testValue normal 3" ~:
        (exRes $ parse value "0xA5" `feed` "") ~?= Right "0xA5"
  ]
