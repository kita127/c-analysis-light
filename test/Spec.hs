{-# LANGUAGE OverloadedStrings #-}
import           Data.Attoparsec.Text           hiding (take)
import           Data.List                      (intercalate)
import qualified Data.Text                      as T
import           Language.C.Analysis.Light
import qualified Language.C.Analysis.Light.Data as DATA
import           Test.HUnit

exRes :: Result a -> Either String a
exRes (Done _ r)    = Right r
exRes (Fail i ss s) = Left $ intercalate " : " ((show i):s:ss)
exRes (Partial _)   = Left "Partial"

main :: IO ()
main = do
    runTestTT $ TestList
      [ testSample
      , testToken
      , testValue
      , testDefVariable
      , testIdentifire
      , testDefFunction
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

-- | testDefFunction input
--

-- void hoge_func( void )
-- {
-- }
--
s_1 = "void hoge_func( void )\n{\n}\n"

-- void
-- arg_func__1(    int arg1   )
-- {
-- }
-- 
s_2 = "void\narg_func__1(    int arg1   )\n{\n}\n"

--      void
--  arg_Thogexxx__1  (    char arg1   )
-- {
-- }
-- 
s_s_1 = "     void\n arg_Thogexxx__1  (    char arg1   )\n{\n}\n"



testDefFunction :: Test
testDefFunction = TestList
  [ "testDefFunction normal 1" ~:
        (exRes $ parse defFunction s_1 `feed` "") ~?= Right
            DATA.Func {
              DATA.return = ["void"]
            , DATA.name   = "hoge_func"
            , DATA.args   = [
                DATA.Var {
                  DATA.typ = ["void"]
                , DATA.name = ""
                , DATA.initVal = Nothing
                }
              ]
            }
  , "testDefFunction normal 2" ~:
        (exRes $ parse defFunction s_2 `feed` "") ~?= Right
            DATA.Func {
              DATA.return = ["void"]
            , DATA.name   = "arg_func__1"
            , DATA.args   = [
                DATA.Var {
                  DATA.typ = ["int"]
                , DATA.name = "arg1"
                , DATA.initVal = Nothing
                }
              ]
            }

  , "testDefFunction space 1" ~:
        (exRes $ parse defFunction s_s_1 `feed` "") ~?= Right
            DATA.Func {
              DATA.return = ["void"]
            , DATA.name   = "arg_Thogexxx__1"
            , DATA.args   = [
                DATA.Var {
                  DATA.typ = ["char"]
                , DATA.name = "arg1"
                , DATA.initVal = Nothing
                }
              ]
            }
  ]

testDefVariable :: Test
testDefVariable = TestList
  [ "testDefVariable normal 1" ~:
        (exRes $ parse defVariable "int hoge;" `feed` "") ~?= Right
            DATA.Var {
              DATA.typ = ["int"]
            , DATA.name = "hoge"
            , DATA.initVal = Nothing
            }
  , "testDefVariable normal 2" ~:
        (exRes $ parse defVariable "MyStruct st_var;" `feed` "") ~?= Right
            DATA.Var {
              DATA.typ = ["MyStruct"]
            , DATA.name = "st_var"
            , DATA.initVal = Nothing
            }
  , "testDefVariable normal 3" ~:
        (exRes $ parse defVariable "unsigned int  uint_var;" `feed` "") ~?= Right
            DATA.Var {
              DATA.typ = ["unsigned", "int"]
            , DATA.name = "uint_var"
            , DATA.initVal = Nothing
            }

  , "testDefVariable initial value 1" ~:
        (exRes $ parse defVariable "Hoge yyy_abc = 100;" `feed` "") ~?= Right
            DATA.Var {
              DATA.typ = ["Hoge"]
            , DATA.name = "yyy_abc"
            , DATA.initVal = Just "100"
            }
  , "testDefVariable initial value 2" ~:
        (exRes $ parse defVariable "Hoge     yyy_abc            =    100       ;" `feed` "") ~?= Right
            DATA.Var {
              DATA.typ = ["Hoge"]
            , DATA.name = "yyy_abc"
            , DATA.initVal = Just "100"
            }
  , "testDefVariable initial value 3" ~:
        (exRes $ parse defVariable "char    foobar_xyz   =  VALUE;" `feed` "") ~?= Right
            DATA.Var {
              DATA.typ = ["char"]
            , DATA.name = "foobar_xyz"
            , DATA.initVal = Just "VALUE"
            }
  , "testDefVariable initial value 4" ~:
        (exRes $ parse defVariable "  static char    foobar_xyz   =  0xFFFF  ;" `feed` "") ~?= Right
            DATA.Var {
              DATA.typ = ["static", "char"]
            , DATA.name = "foobar_xyz"
            , DATA.initVal = Just "0xFFFF"
            }

  , "testDefVariable pointer 1" ~:
        (exRes $ parse defVariable "  signed int    *p_val_axz   =  &hoge  ;" `feed` "") ~?= Right
            DATA.Var {
              DATA.typ = ["signed", "int", "*"]
            , DATA.name = "p_val_axz"
            , DATA.initVal = Just "&hoge"
            }
  , "testDefVariable pointer 2" ~:
        (exRes $ parse defVariable "  signed  * int    **p_val_00d4   =  &hoge  ;" `feed` "") ~?= Right
            DATA.Var {
              DATA.typ = ["signed", "*", "int", "*", "*"]
            , DATA.name = "p_val_00d4"
            , DATA.initVal = Just "&hoge"
            }
  ]

testIdentifire :: Test
testIdentifire = TestList
  [ "testIdentifire normal 1" ~:
        (exRes $ parse identifire "hoge_var" `feed` "") ~?= Right "hoge_var"
  , "testIdentifire include number 1" ~:
        (exRes $ parse identifire "bar123hoge" `feed` "") ~?= Right "bar123hoge"
  , "testIdentifire first letter which is number 1" ~:
        (exRes $ parse identifire "999_error" `feed` "") ~?= Left "\"999_error\" : Failed reading: satisfy : '_'"
  ]



testValue :: Test
testValue = TestList
  [ "testValue normal 1" ~:
        (exRes $ parse value "VALUE" `feed` "") ~?= Right "VALUE"
  , "testValue normal 2" ~:
        (exRes $ parse value "234" `feed` "") ~?= Right "234"
  , "testValue hex 1" ~:
        (exRes $ parse value "0xA5" `feed` "") ~?= Right "0xA5"
  , "testValue oct 1" ~:
        (exRes $ parse value "036" `feed` "") ~?= Right "036"
  , "testValue address 1" ~:
        (exRes $ parse value "&hoge" `feed` "") ~?= Right "&hoge"
  ]
