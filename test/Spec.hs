{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
import           Control.Monad.Trans.State
import           Data.Attoparsec.Text           hiding (take)
import           Data.List                      (intercalate)
import qualified Data.Text                      as T
import           Language.C.Analysis.Light
import qualified Language.C.Analysis.Light.Data as DATA
import           Test.HUnit
import           Text.RawString.QQ

exRes :: Result a -> Either String a
exRes (Done _ r)    = Right r
exRes (Fail i ss s) = Left $ intercalate " : " ((show i):s:ss)
exRes (Partial _)   = Left "Partial"

stParse s p i = let p' = (`evalStateT` s) p in parse p' i

main :: IO ()
main = do
    runTestTT $ TestList
      [ testSample
      , testToken
      , testComment
      , testValue
      , testIdentifire
      , testDefVariable
      --, testDefFunction
      --, testArguments

      --, testPreprocess

      --, testJustPreIf
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
        (exRes $ stParse [] (token identifire) "    hoge_var    " `feed` "") ~?= Right "hoge_var"
  , "testToken normal 2" ~:
        (exRes $ stParse [] (token identifire) "    hoge_var\n    " `feed` "") ~?= Right "hoge_var"
  ]


-- | testComment
--
testComment :: Test
testComment = TestList
  [ "testComment normal 1" ~:
        (exRes $ stParse [] defVariable "/* comment */    MyType my_var = tmp_v;" `feed` "") ~?= Right
            DATA.Var {
              DATA.prepro = []
            , DATA.typ = ["MyType"]
            , DATA.name = "my_var"
            , DATA.initVal = Just "tmp_v"
            }
  , "testComment normal 2" ~:
        (exRes $ stParse [] defVariable "    // comment\nMyType my_var = tmp_v;" `feed` "") ~?= Right
            DATA.Var {
              DATA.prepro = []
            , DATA.typ = ["MyType"]
            , DATA.name = "my_var"
            , DATA.initVal = Just "tmp_v"
            }
  ]



testValue :: Test
testValue = TestList
  [ "testValue normal 1" ~:
        (exRes $ stParse [] value "VALUE" `feed` "") ~?= Right "VALUE"
  , "testValue normal 2" ~:
        (exRes $ stParse [] value "234" `feed` "") ~?= Right "234"
  , "testValue hex 1" ~:
        (exRes $ stParse [] value "0xA5" `feed` "") ~?= Right "0xA5"
  , "testValue oct 1" ~:
        (exRes $ stParse [] value "036" `feed` "") ~?= Right "036"
  , "testValue address 1" ~:
        (exRes $ stParse [] value "&hoge" `feed` "") ~?= Right "&hoge"
  ]



testIdentifire :: Test
testIdentifire = TestList
  [ "testIdentifire normal 1" ~:
        (exRes $ stParse [] identifire "hoge_var" `feed` "") ~?= Right "hoge_var"
  , "testIdentifire include number 1" ~:
        (exRes $ stParse [] identifire "bar123hoge" `feed` "") ~?= Right "bar123hoge"
  , "testIdentifire first letter which is number 1" ~:
        (exRes $ stParse [] identifire "999_error" `feed` "") ~?= Left "\"999_error\" : Failed reading: satisfy : '_'"
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

-- static int * mul_ret_arg_f ( char hoge, int *p_fuga )
-- {
-- }
--
s_3 = "static int * mul_ret_arg_f ( char hoge, int *p_fuga )\n{\n}\n"

s_4 = [r|
int main( void )
{
    printf("Hellow World\n");

    return (0);
}
|]

----      void
----  arg_Thogexxx__1  (    char arg1   )
---- {
---- }
----
--s_s_1 = "     void\n arg_Thogexxx__1  (    char arg1   )\n{\n}\n"
--
--
--
--testDefFunction :: Test
--testDefFunction = TestList
--  [ "testDefFunction normal 1" ~:
--        (exRes $ parse (defFunction []) s_1 `feed` "") ~?= Right
--            DATA.Func {
--              DATA.return = ["void"]
--            , DATA.name   = "hoge_func"
--            , DATA.args   = [
--                DATA.Var {
--                  DATA.typ = ["void"]
--                , DATA.name = ""
--                , DATA.initVal = Nothing
--                }
--              ]
--            , DATA.procs = []
--            }
--  , "testDefFunction normal 2" ~:
--        (exRes $ parse (defFunction []) s_2 `feed` "") ~?= Right
--            DATA.Func {
--              DATA.return = ["void"]
--            , DATA.name   = "arg_func__1"
--            , DATA.args   = [
--                DATA.Var {
--                  DATA.typ = ["int"]
--                , DATA.name = "arg1"
--                , DATA.initVal = Nothing
--                }
--              ]
--            , DATA.procs = []
--            }
--
---- static int * mul_ret_arg_f ( char hoge, int *p_fuga )
---- {
---- }
----
--  , "testDefFunction normal 3" ~:
--        (exRes $ parse (defFunction []) s_3 `feed` "") ~?= Right
--            DATA.Func {
--              DATA.return = ["static", "int", "*"]
--            , DATA.name   = "mul_ret_arg_f"
--            , DATA.args   = [
--                DATA.Var {
--                  DATA.typ = ["char"]
--                , DATA.name = "hoge"
--                , DATA.initVal = Nothing
--                }
--              , DATA.Var {
--                  DATA.typ = ["int", "*"]
--                , DATA.name = "p_fuga"
--                , DATA.initVal = Nothing
--                }
--              ]
--            , DATA.procs = []
--            }
--
--  , "testDefFunction normal 4" ~:
--        (exRes $ parse (defFunction []) s_4 `feed` "") ~?= Right
--            DATA.Func {
--              DATA.return = ["int"]
--            , DATA.name   = "main"
--            , DATA.args   = [
--                DATA.Var {
--                  DATA.typ = ["void"]
--                , DATA.name = ""
--                , DATA.initVal = Nothing
--                }
--              ]
--            , DATA.procs = [
--                DATA.Call {
--                  DATA.prepro = []
--                , DATA.name = "printf"
--                , DATA.args = ["\"Hellow World\\n\""]
--                }
--              , DATA.Return {
--                  DATA.prepro = []
--                , DATA.value = "0"
--                }
--              ]
--            }
--
--  , "testDefFunction space 1" ~:
--        (exRes $ parse (defFunction []) s_s_1 `feed` "") ~?= Right
--            DATA.Func {
--              DATA.return = ["void"]
--            , DATA.name   = "arg_Thogexxx__1"
--            , DATA.args   = [
--                DATA.Var {
--                  DATA.typ = ["char"]
--                , DATA.name = "arg1"
--                , DATA.initVal = Nothing
--                }
--              ]
--            , DATA.procs = []
--            }
--  ]


testDefVariable_in1 = [r|
#if HOGE_SW == 1

char condition_variable;

#endif    /* HOGE_SW */
|]


testDefVariable :: Test
testDefVariable = TestList
  [ "testDefVariable normal 1" ~:
        (exRes $ stParse [] defVariable "int hoge;" `feed` "") ~?= Right
            DATA.Var {
              DATA.prepro = []
            , DATA.typ = ["int"]
            , DATA.name = "hoge"
            , DATA.initVal = Nothing
            }
  , "testDefVariable normal 2" ~:
        (exRes $ stParse [] defVariable "MyStruct st_var;" `feed` "") ~?= Right
            DATA.Var {
              DATA.prepro = []
            , DATA.typ = ["MyStruct"]
            , DATA.name = "st_var"
            , DATA.initVal = Nothing
            }
  , "testDefVariable normal 3" ~:
        (exRes $ stParse [] defVariable "unsigned int  uint_var;" `feed` "") ~?= Right
            DATA.Var {
              DATA.prepro = []
            , DATA.typ = ["unsigned", "int"]
            , DATA.name = "uint_var"
            , DATA.initVal = Nothing
            }

  , "testDefVariable initial value 1" ~:
        (exRes $ stParse [] defVariable "Hoge yyy_abc = 100;" `feed` "") ~?= Right
            DATA.Var {
              DATA.prepro = []
            , DATA.typ = ["Hoge"]
            , DATA.name = "yyy_abc"
            , DATA.initVal = Just "100"
            }
  , "testDefVariable initial value 2" ~:
        (exRes $ stParse [] defVariable "Hoge     yyy_abc            =    100       ;" `feed` "") ~?= Right
            DATA.Var {
              DATA.prepro = []
            , DATA.typ = ["Hoge"]
            , DATA.name = "yyy_abc"
            , DATA.initVal = Just "100"
            }
  , "testDefVariable initial value 3" ~:
        (exRes $ stParse [] defVariable "char    foobar_xyz   =  VALUE;" `feed` "") ~?= Right
            DATA.Var {
              DATA.prepro = []
            , DATA.typ = ["char"]
            , DATA.name = "foobar_xyz"
            , DATA.initVal = Just "VALUE"
            }
  , "testDefVariable initial value 4" ~:
        (exRes $ stParse [] defVariable "  static char    foobar_xyz   =  0xFFFF  ;" `feed` "") ~?= Right
            DATA.Var {
              DATA.prepro = []
            , DATA.typ = ["static", "char"]
            , DATA.name = "foobar_xyz"
            , DATA.initVal = Just "0xFFFF"
            }

  , "testDefVariable pointer 1" ~:
        (exRes $ stParse [] defVariable "  signed int    *p_val_axz   =  &hoge  ;" `feed` "") ~?= Right
            DATA.Var {
              DATA.prepro = []
            , DATA.typ = ["signed", "int", "*"]
            , DATA.name = "p_val_axz"
            , DATA.initVal = Just "&hoge"
            }
  , "testDefVariable pointer 2" ~:
        (exRes $ stParse [] defVariable "  signed  * int    **p_val_00d4   =  &hoge  ;" `feed` "") ~?= Right
            DATA.Var {
              DATA.prepro = []
            , DATA.typ = ["signed", "*", "int", "*", "*"]
            , DATA.name = "p_val_00d4"
            , DATA.initVal = Just "&hoge"
            }
  , "testDefVariable prepro 1" ~:
        (exRes $ stParse [] defVariable testDefVariable_in1 `feed` "") ~?= Right
            DATA.Var {
              DATA.prepro = [
                DATA.Condition {
                  DATA.command = "#if"
                , DATA.left = "HOGE_SW"
                , DATA.op = "=="
                , DATA.right = "1"
                }
            ]
            , DATA.typ = ["char"]
            , DATA.name = "condition_variable"
            , DATA.initVal = Nothing
            }
  ]





--testArguments :: Test
--testArguments = TestList
--  [ "testArguments normal 1" ~:
--        (exRes $ parse arguments "  int arg " `feed` "") ~?= Right
--            [
--              DATA.Var {
--                DATA.typ = ["int"]
--              , DATA.name = "arg"
--              , DATA.initVal = Nothing
--              }
--            ]
--  ]
--
--
--
---- | test preproIfStart
----
--
--
---- #if PRE_VARI == 1
---- signed int pre_var;
---- unsigned  char pre_var2;
---- #endif
----
--s_preIf_1 = "#if PRE_VARI == 1\nsigned int pre_var;\nunsigned  char pre_var2;\n#endif\n"
--
--
---- #if PRE_VARI == 1
---- signed int pre_var;
---- unsigned  char pre_var2;
---- #endif
---- int normal_var  =  55;
----
--s_preIf_2 = "    #if PRE_VARI == 1\nsigned int pre_var;\nunsigned  char pre_var2;\n    #endif    /* PRE_VARI */\nint normal_var  =  55;\n"
--
--
--testJustPreIf :: Test
--testJustPreIf = TestList
--  [ "testJustPreIf normal 1" ~:
--        (exRes $ parse preproIfStart "#if PRE_VARI == 1\nint hoge;\n#endif" `feed` "") ~?= Right
--            DATA.Csrc {
--              DATA.prepro = ["#if PRE_VARI == 1"]
--            , DATA.statements = DATA.Var {
--                DATA.typ = ["int"]
--              , DATA.name = "hoge"
--              , DATA.initVal = Nothing
--              }
--            , DATA.next = DATA.End
--            }
--  , "testJustPreIf normal 2" ~:
--        (exRes $ parse preproIfStart s_preIf_1 `feed` "") ~?= Right
--            DATA.Csrc {
--              DATA.prepro = ["#if PRE_VARI == 1"]
--            , DATA.statements = DATA.Var {
--                DATA.typ = ["signed", "int"]
--              , DATA.name = "pre_var"
--              , DATA.initVal = Nothing
--              }
--            , DATA.next = DATA.Csrc {
--                DATA.prepro = ["#if PRE_VARI == 1"]
--              , DATA.statements = DATA.Var {
--                  DATA.typ = ["unsigned", "char"]
--                , DATA.name = "pre_var2"
--                , DATA.initVal = Nothing
--                }
--              , DATA.next = DATA.End
--              }
--            }
--  , "testJustPreIf normal 3" ~:
--        (exRes $ parse preproIfStart s_preIf_2 `feed` "") ~?= Right
--            DATA.Csrc {
--              DATA.prepro = ["#if PRE_VARI == 1"]
--            , DATA.statements = DATA.Var {
--                DATA.typ = ["signed", "int"]
--              , DATA.name = "pre_var"
--              , DATA.initVal = Nothing
--              }
--            , DATA.next = DATA.Csrc {
--                DATA.prepro = ["#if PRE_VARI == 1"]
--              , DATA.statements = DATA.Var {
--                  DATA.typ = ["unsigned", "char"]
--                , DATA.name = "pre_var2"
--                , DATA.initVal = Nothing
--                }
--              , DATA.next = DATA.Csrc {
--                  DATA.prepro = []
--                , DATA.statements = DATA.Var {
--                    DATA.typ = ["int"]
--                  , DATA.name = "normal_var"
--                  , DATA.initVal = Just "55"
--                  }
--                , DATA.next = DATA.End
--                }
--              }
--            }
--  , "testJustPreIf normal 4" ~:
--        (exRes $ parse preproIfStart "\n#if HOGE_XXX == VARI1 \nint hoge;\n#endif    /* HOGE_XXX */\n" `feed` "") ~?= Right
--            DATA.Csrc {
--              DATA.prepro = ["#if HOGE_XXX == VARI1"]
--            , DATA.statements = DATA.Var {
--                DATA.typ = ["int"]
--              , DATA.name = "hoge"
--              , DATA.initVal = Nothing
--              }
--            , DATA.next = DATA.End
--            }
--  ]
--
--
--
--
--
--
--
--
---- | testPreprocess
----
--testPreprocess :: Test
--testPreprocess = TestList
--  [ "testPreprocess include 1" ~:
--        (exRes $ parse (preprocess []) "#include <stdio.h>\n" `feed` "") ~?= Right
--            DATA.Prepro {
--              DATA.prepro = []
--            , DATA.contents = DATA.Include {
--                DATA.file = "<stdio.h>"
--              }
--            , DATA.next = DATA.End
--            }
--  ]
