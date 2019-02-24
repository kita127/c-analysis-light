{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
import           Control.Monad.Trans.State
import           Data.Attoparsec.Text           hiding (take)
import           Data.List                      (intercalate)
import qualified Data.Text                      as T
import           Language.C.Analysis.Light
import qualified Language.C.Analysis.Light.Data as D
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
      , testDefFunction
      , testInclude
      , testExpr
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
            D.Var {
              D.prepro = []
            , D.typ = ["MyType"]
            , D.name = "my_var"
            , D.initVal = Just "tmp_v"
            }
  , "testComment normal 2" ~:
        (exRes $ stParse [] defVariable "    // comment\nMyType my_var = tmp_v;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["MyType"]
            , D.name = "my_var"
            , D.initVal = Just "tmp_v"
            }
  ]


-- | testInclude
--
testInclude :: Test
testInclude = TestList
  [ "testInclude normal 1" ~:
        (exRes $ stParse [] include "#include    <stdio.h>      \n" `feed` "") ~?= Right
            D.Include {
              D.prepro = []
            , D.file = "<stdio.h>"
            }
  , "testInclude error 1" ~:
        (exRes $ stParse [] include "#include    <stdio.h>" `feed` "") ~?= Left "\"\" : not enough input"
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
-- | 1
--
testDefFunction_in1 = [r|
void hoge_func( void )
{
}
|]

-- | 2
--
testDefFunction_in2 = [r|
void hoge_func__1(    int arg1   )
{

}
|]






testDefFunction :: Test
testDefFunction = TestList
  [ "testDefFunction normal 1" ~:
        (exRes $ stParse [] defFunction testDefFunction_in1 `feed` "") ~?= Right
            D.Func {
              D.prepro = []
            , D.return = ["void"]
            , D.name   = "hoge_func"
            , D.args   = [
                D.Var {
                  D.prepro = []
                , D.typ = ["void"]
                , D.name = ""
                , D.initVal = Nothing
                }
              ]
            , D.procs = []
            }
  , "testDefFunction normal 2" ~:
        (exRes $ stParse [] defFunction testDefFunction_in2 `feed` "") ~?= Right
            D.Func {
              D.prepro = []
            , D.return = ["void"]
            , D.name   = "hoge_func__1"
            , D.args   = [
                D.Var {
                  D.prepro = []
                , D.typ = ["int"]
                , D.name = "arg1"
                , D.initVal = Nothing
                }
              ]
            , D.procs = []
            }

  , "testDefFunction normal 3" ~:
        (exRes $ stParse [] defFunction [r|
static int * mul_ret_arg_f ( char hoge, int *p_fuga )
{
}
|] `feed` "") ~?= Right
            D.Func {
              D.prepro = []
            , D.return = ["static", "int", "*"]
            , D.name   = "mul_ret_arg_f"
            , D.args   = [
                D.Var {
                  D.prepro = []
                , D.typ = ["char"]
                , D.name = "hoge"
                , D.initVal = Nothing
                }
              , D.Var {
                  D.prepro = []
                , D.typ = ["int", "*"]
                , D.name = "p_fuga"
                , D.initVal = Nothing
                }
              ]
            , D.procs = []
            }

  , "testDefFunction call 1" ~:
        (exRes $ stParse [] defFunction [r|
int main( void )
{
    printf("Hellow World\n");

    return (0);
}
|] `feed` "") ~?= Right
            D.Func {
              D.prepro = []
            , D.return = ["int"]
            , D.name   = "main"
            , D.args   = [
                D.Var {
                  D.prepro = []
                , D.typ = ["void"]
                , D.name = ""
                , D.initVal = Nothing
                }
              ]
            , D.procs = [
                D.Call {
                  D.prepro = []
                , D.name = "printf"
                , D.args = [
                    D.StrLiteral {D.str = "Hellow World\\n"}
                  ]
                }
              , D.Return {
                  D.prepro = []
                , D.value = "0"
                }
              ]
            }

  , "testDefFunction call 2" ~:
        (exRes $ stParse [] defFunction [r|
int main( void )
{
    printf("local_var ...%d\n", local_var);

    return (0);
}
|] `feed` "") ~?= Right
            D.Func {
              D.prepro = []
            , D.return = ["int"]
            , D.name   = "main"
            , D.args   = [
                D.Var {
                  D.prepro = []
                , D.typ = ["void"]
                , D.name = ""
                , D.initVal = Nothing
                }
              ]
            , D.procs = [
                D.Call {
                  D.prepro = []
                , D.name = "printf"
                , D.args = [
                    D.StrLiteral {D.str = "local_var ...%d\\n"}
                  , D.Identifire {D.id = "local_var"}
                  ]
                }
              , D.Return {
                  D.prepro = []
                , D.value = "0"
                }
              ]
            }

  , "testDefFunction local var 1" ~:
        (exRes $ stParse [] defFunction [r|
void func( void )
{
    int local_var;
}
|] `feed` "") ~?= Right
            D.Func {
              D.prepro = []
            , D.return = ["void"]
            , D.name   = "func"
            , D.args   = [
                D.Var {
                  D.prepro = []
                , D.typ = ["void"]
                , D.name = ""
                , D.initVal = Nothing
                }
              ]
            , D.procs = [
                D.LVar {
                  D.prepro = []
                , D.var = D.Var {
                    D.prepro = []
                  , D.typ = ["int"]
                  , D.name = "local_var"
                  , D.initVal = Nothing
                  }
                }
              ]
            }

  , "testDefFunction assigne 1" ~:
        (exRes $ stParse [] defFunction [r|
void func( void )
{
    local_var = 2;
}
|] `feed` "") ~?= Right
            D.Func {
              D.prepro = []
            , D.return = ["void"]
            , D.name   = "func"
            , D.args   = [
                D.Var {
                  D.prepro = []
                , D.typ = ["void"]
                , D.name = ""
                , D.initVal = Nothing
                }
              ]
            , D.procs = [
                D.Assigne{
                  D.prepro = []
                , D.left = "local_var"
                , D.right = D.Literal {value = "2"}
                }
              ]
            }
  ]


testDefVariable_in1 = [r|
#if HOGE_SW == 1

char condition_variable;

#endif    /* HOGE_SW */
|]


testDefVariable :: Test
testDefVariable = TestList
  [ "testDefVariable normal 1" ~:
        (exRes $ stParse [] defVariable "int hoge;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["int"]
            , D.name = "hoge"
            , D.initVal = Nothing
            }
  , "testDefVariable normal 2" ~:
        (exRes $ stParse [] defVariable "MyStruct st_var;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["MyStruct"]
            , D.name = "st_var"
            , D.initVal = Nothing
            }
  , "testDefVariable normal 3" ~:
        (exRes $ stParse [] defVariable "unsigned int  uint_var;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["unsigned", "int"]
            , D.name = "uint_var"
            , D.initVal = Nothing
            }

  , "testDefVariable initial value 1" ~:
        (exRes $ stParse [] defVariable "Hoge yyy_abc = 100;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["Hoge"]
            , D.name = "yyy_abc"
            , D.initVal = Just "100"
            }
  , "testDefVariable initial value 2" ~:
        (exRes $ stParse [] defVariable "Hoge     yyy_abc            =    100       ;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["Hoge"]
            , D.name = "yyy_abc"
            , D.initVal = Just "100"
            }
  , "testDefVariable initial value 3" ~:
        (exRes $ stParse [] defVariable "char    foobar_xyz   =  VALUE;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["char"]
            , D.name = "foobar_xyz"
            , D.initVal = Just "VALUE"
            }
  , "testDefVariable initial value 4" ~:
        (exRes $ stParse [] defVariable "  static char    foobar_xyz   =  0xFFFF  ;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["static", "char"]
            , D.name = "foobar_xyz"
            , D.initVal = Just "0xFFFF"
            }

  , "testDefVariable pointer 1" ~:
        (exRes $ stParse [] defVariable "  signed int    *p_val_axz   =  &hoge  ;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["signed", "int", "*"]
            , D.name = "p_val_axz"
            , D.initVal = Just "&hoge"
            }
  , "testDefVariable pointer 2" ~:
        (exRes $ stParse [] defVariable "  signed  * int    **p_val_00d4   =  &hoge  ;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["signed", "*", "int", "*", "*"]
            , D.name = "p_val_00d4"
            , D.initVal = Just "&hoge"
            }
  , "testDefVariable prepro 1" ~:
        (exRes $ stParse [] defVariable testDefVariable_in1 `feed` "") ~?= Right
            D.Var {
              D.prepro = [
                D.Condition {
                  D.command = "#if"
                , D.left = "HOGE_SW"
                , D.op = "=="
                , D.right = "1"
                }
            ]
            , D.typ = ["char"]
            , D.name = "condition_variable"
            , D.initVal = Nothing
            }
  ]

-- | testExpr
--
testExpr :: Test
testExpr = TestList
  [ "testExpr literal 1" ~:
        (exRes $ stParse [] expr "123" `feed` "") ~?= Right
            D.Literal {
              D.value = "123"
            }
  , "testExpr literal space 1" ~:
        (exRes $ stParse [] expr "   123    " `feed` "") ~?= Right
            D.Literal {
              D.value = "123"
            }
  , "testExpr addition 1" ~:
        (exRes $ stParse [] expr "1 + 2" `feed` "") ~?= Right
            D.Binary {
              D.op = D.Add
            , D.left = D.Literal {
                D.value = "1"
              }
            , D.right = D.Literal {
                D.value = "2"
              }
            }



  ]




--testArguments :: Test
--testArguments = TestList
--  [ "testArguments normal 1" ~:
--        (exRes $ parse arguments "  int arg " `feed` "") ~?= Right
--            [
--              D.Var {
--                D.typ = ["int"]
--              , D.name = "arg"
--              , D.initVal = Nothing
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
--            D.Csrc {
--              D.prepro = ["#if PRE_VARI == 1"]
--            , D.statements = D.Var {
--                D.typ = ["int"]
--              , D.name = "hoge"
--              , D.initVal = Nothing
--              }
--            , D.next = D.End
--            }
--  , "testJustPreIf normal 2" ~:
--        (exRes $ parse preproIfStart s_preIf_1 `feed` "") ~?= Right
--            D.Csrc {
--              D.prepro = ["#if PRE_VARI == 1"]
--            , D.statements = D.Var {
--                D.typ = ["signed", "int"]
--              , D.name = "pre_var"
--              , D.initVal = Nothing
--              }
--            , D.next = D.Csrc {
--                D.prepro = ["#if PRE_VARI == 1"]
--              , D.statements = D.Var {
--                  D.typ = ["unsigned", "char"]
--                , D.name = "pre_var2"
--                , D.initVal = Nothing
--                }
--              , D.next = D.End
--              }
--            }
--  , "testJustPreIf normal 3" ~:
--        (exRes $ parse preproIfStart s_preIf_2 `feed` "") ~?= Right
--            D.Csrc {
--              D.prepro = ["#if PRE_VARI == 1"]
--            , D.statements = D.Var {
--                D.typ = ["signed", "int"]
--              , D.name = "pre_var"
--              , D.initVal = Nothing
--              }
--            , D.next = D.Csrc {
--                D.prepro = ["#if PRE_VARI == 1"]
--              , D.statements = D.Var {
--                  D.typ = ["unsigned", "char"]
--                , D.name = "pre_var2"
--                , D.initVal = Nothing
--                }
--              , D.next = D.Csrc {
--                  D.prepro = []
--                , D.statements = D.Var {
--                    D.typ = ["int"]
--                  , D.name = "normal_var"
--                  , D.initVal = Just "55"
--                  }
--                , D.next = D.End
--                }
--              }
--            }
--  , "testJustPreIf normal 4" ~:
--        (exRes $ parse preproIfStart "\n#if HOGE_XXX == VARI1 \nint hoge;\n#endif    /* HOGE_XXX */\n" `feed` "") ~?= Right
--            D.Csrc {
--              D.prepro = ["#if HOGE_XXX == VARI1"]
--            , D.statements = D.Var {
--                D.typ = ["int"]
--              , D.name = "hoge"
--              , D.initVal = Nothing
--              }
--            , D.next = D.End
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
--            D.Prepro {
--              D.prepro = []
--            , D.contents = D.Include {
--                D.file = "<stdio.h>"
--              }
--            , D.next = D.End
--            }
--  ]
