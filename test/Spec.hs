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
      , testIdentifire
      , testDefVariable
      , testDefFunction
      , testInclude
      , testDefine
      , testExpr
      --, testArguments

      --, testPreprocess

      --, testJustPreIf
      , testProgram
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
            , D.initVal = Just (
                D.Identifire {
                  D.name = "tmp_v"
                }
              )
            }
  , "testComment normal 2" ~:
        (exRes $ stParse [] defVariable "    // comment\nMyType my_var = tmp_v;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["MyType"]
            , D.name = "my_var"
            , D.initVal = Just (
                D.Identifire {
                  D.name = "tmp_v"
                }
              )
            }
  ]


-- | testInclude
--
testInclude :: Test
testInclude = TestList
  [ "testInclude normal 1" ~:
        (exRes $ stParse [] preprocess "#include    <stdio.h>      \n" `feed` "") ~?= Right
            D.Preprocess {
              D.contents = D.Include {
                D.prepro = []
              , D.file = "<stdio.h>"
              }
            }
  , "testInclude error 1" ~:
        (exRes $ stParse [] preprocess "#include    <stdio.h>" `feed` "") ~?= Left "\"#include    <stdio.h>\" : string"
  ]

-- | testDefine
--
testDefine :: Test
testDefine = TestList
  [ "testDefine normal 1" ~:
        (exRes $ stParse [] preprocess "#define CONDITION       HOGE\n" `feed` "") ~?= Right
            D.Preprocess {
              D.contents = D.Define {
                D.prepro = []
              , D.name = "CONDITION"
              , D.value = "HOGE"
              }
            }

  , "testDefine normal 2" ~:
        (exRes $ stParse [] preprocess "    #define CONDITION       HOGE\n" `feed` "") ~?= Right
            D.Preprocess {
              D.contents = D.Define {
                D.prepro = []
              , D.name = "CONDITION"
              , D.value = "HOGE"
              }
            }


  , "testDefine error less CR" ~:
        (exRes $ stParse [] preprocess "#define    HOGE    VAR" `feed` "") ~?= Left "\"\" : not enough input"

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


-- | testDefFunction
--
testDefFunction :: Test
testDefFunction = TestList
  [ "testDefFunction normal 1" ~:
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

  , "testDefFunction expression expState 1" ~:
        (exRes $ stParse [] defFunction [r|
int func( void )
{
    1 + 2;
}
|] `feed` "") ~?= Right
            D.Func {
              D.prepro = []
            , D.return = ["int"]
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
                D.ExpState {
                  D.prepro = []
                , D.contents = D.Binary {
                    D.op = "+"
                  , D.left = D.Literal {
                      D.value = "1"
                    }
                  , D.right = D.Literal {
                      D.value = "2"
                    }
                  }
                }
              ]
            }

  , "testDefFunction local var 1" ~:
        (exRes $ stParse [] defFunction [r|
int func( void )
{
    char local_var = VALUE;
}
|] `feed` "") ~?= Right
            D.Func {
              D.prepro = []
            , D.return = ["int"]
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
                  , D.typ = ["char"]
                  , D.name = "local_var"
                  , D.initVal = Just (
                      D.Identifire {
                        D.name = "VALUE"
                      }
                    )
                  }
                }
              ]
            }

  , "testDefFunction return 1" ~:
        (exRes $ stParse [] defFunction [r|
int func( void )
{
    return (100 + 51);
}
|] `feed` "") ~?= Right
            D.Func {
              D.prepro = []
            , D.return = ["int"]
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
                D.Return {
                  D.prepro = []
                , D.operand = D.Binary {
                    D.op = "+"
                  , D.left = D.Literal {
                      D.value = "100"
                    }
                  , D.right = D.Literal {
                      D.value = "51"
                    }
                  }
                }
              ]
            }


  ]



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
            , D.initVal = Just (
                D.Literal {
                  D.value = "100"
                }
              )
            }
  , "testDefVariable initial value 2" ~:
        (exRes $ stParse [] defVariable "Hoge     yyy_abc            =    100       ;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["Hoge"]
            , D.name = "yyy_abc"
            , D.initVal = Just (
                D.Literal {
                  D.value = "100"
                }
              )
            }
  , "testDefVariable initial value 3" ~:
        (exRes $ stParse [] defVariable "char    foobar_xyz   =  VALUE;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["char"]
            , D.name = "foobar_xyz"
            , D.initVal = Just (
                D.Identifire {
                  D.name = "VALUE"
                }
              )
            }
  , "testDefVariable initial value 4" ~:
        (exRes $ stParse [] defVariable "  static char    foobar_xyz   =  0xFFFF  ;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["static", "char"]
            , D.name = "foobar_xyz"
            , D.initVal = Just (
                D.Literal {
                  D.value = "0xFFFF"
                }
              )
            }

  , "testDefVariable pointer 1" ~:
        (exRes $ stParse [] defVariable "  signed int    *p_val_axz   =  &hoge  ;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["signed", "int", "*"]
            , D.name = "p_val_axz"
            , D.initVal = Just (
                D.PreUnary {
                  D.op = "&"
                , D.operand = D.Identifire {
                    D.name = "hoge"
                  }
                }
              )
            }
  , "testDefVariable pointer 2" ~:
        (exRes $ stParse [] defVariable "  signed  * int    **p_val_00d4   =  &hoge  ;" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["signed", "*", "int", "*", "*"]
            , D.name = "p_val_00d4"
            , D.initVal = Just (
                D.PreUnary {
                  D.op = "&"
                , D.operand = D.Identifire {
                    D.name = "hoge"
                  }
                }
              )
            }

  , "testDefVariable prepro 1" ~:
        (exRes $ stParse [] defVariable [r|
#if HOGE_SW == 1

char condition_variable;

#endif    /* HOGE_SW */
|] `feed` "") ~?= Right
            D.Var {
              D.prepro = [
                D.Condition {
                  D.command = "#if"
                , D.expr = D.Binary {
                    D.op = "=="
                  , D.left = D.Identifire {
                      D.name = "HOGE_SW"
                    }
                  , D.right = D.Literal {
                      D.value = "1"
                    }
                  }
                }
              ]
            , D.typ = ["char"]
            , D.name = "condition_variable"
            , D.initVal = Nothing
            }
  , "testDefVariable array 1" ~:
        (exRes $ stParse [] defVariable "  int    var_arr[10];" `feed` "") ~?= Right
            D.Var {
              D.prepro = []
            , D.typ = ["int", "[10]"]
            , D.name = "var_arr"
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

  , "testExpr literal + space 1" ~:
        (exRes $ stParse [] expr "   123    " `feed` "") ~?= Right
            D.Literal {
              D.value = "123"
            }

  , "testExpr str literal 1" ~:
        (exRes $ stParse [] expr [r|"Hello world\n"|] `feed` "") ~?= Right
            D.StrLiteral {
              D.value = "Hello world\\n"
            }

  , "testExpr ampersand 1" ~:
        (exRes $ stParse [] expr "&hoge" `feed` "") ~?= Right
            D.PreUnary {
              D.op = "&"
            , D.operand = D.Identifire {
                D.name = "hoge"
              }
            }

  , "testExpr addition 1" ~:
        (exRes $ stParse [] expr "1 + 2" `feed` "") ~?= Right
            D.Binary {
              D.op = "+"
            , D.left = D.Literal {
                D.value = "1"
              }
            , D.right = D.Literal {
                D.value = "2"
              }
            }

  , "testExpr addition 1" ~:
        (exRes $ stParse [] expr "1 + 2 + 3" `feed` "") ~?= Right
            D.Binary {
              D.op = "+"
            , D.left = D.Binary {
                D.op = "+"
              , D.left = D.Literal {
                  D.value = "1"
                }
              , D.right = D.Literal {
                  D.value = "2"
                }
              }
            , D.right = D.Literal {
                D.value = "3"
              }
            }

  , "testExpr addition + parens 1" ~:
        (exRes $ stParse [] expr "1 + (2 + 3)" `feed` "") ~?= Right
            D.Binary {
              D.op = "+"
            , D.left = D.Literal {
                D.value = "1"
              }
            , D.right = D.Binary {
                D.op = "+"
              , D.left = D.Literal {
                  D.value = "2"
                }
              , D.right = D.Literal {
                  D.value = "3"
                }
              }
            }


  , "testExpr  identifire 1" ~:
        (exRes $ stParse [] expr "hoge_var" `feed` "") ~?= Right
            D.Identifire {
              D.name = "hoge_var"
            }

  , "testExpr  call 1" ~:
        (exRes $ stParse [] expr "func_xxx(abc, 1 + 2)" `feed` "") ~?= Right
            D.Call {
              D.func = "func_xxx"
            , D.args = [
                D.Identifire {
                  D.name = "abc"
                }
              , D.Binary {
                  D.op = "+"
                , D.left = D.Literal {
                    D.value = "1"
                  }
                , D.right = D.Literal {
                    D.value = "2"
                  }
                }
              ]
            }

  , "testExpr  assigne 1" ~:
        (exRes $ stParse [] expr "hoge = 99 - 5" `feed` "") ~?= Right
            D.Binary {
              D.op = "="
            , D.left = D.Identifire {
                D.name = "hoge"
              }
            , D.right = D.Binary {
                D.op = "-"
              , D.left = D.Literal {
                  D.value = "99"
                }
              , D.right = D.Literal {
                  D.value = "5"
                }
              }
            }

  , "testExpr  multicalc 1" ~:
        (exRes $ stParse [] expr "1 * 2 + 3 / (4 - 5)" `feed` "") ~?= Right
            D.Binary {
              D.op = "+"
            , D.left = D.Binary {
                D.op = "*"
              , D.left = D.Literal {
                  D.value = "1"
                }
              , D.right = D.Literal {
                  D.value = "2"
                }
              }
            , D.right = D.Binary {
                D.op = "/"
              , D.left = D.Literal {
                  D.value = "3"
                }
              , D.right = D.Binary {
                  D.op = "-"
                , D.left = D.Literal {
                    D.value = "4"
                  }
                , D.right = D.Literal {
                    D.value = "5"
                  }
                }
              }
            }

  , "testExpr  multicalc 2" ~:
        (exRes $ stParse [] expr "hoge * 2 + fuga / (4 - 5)" `feed` "") ~?= Right
            D.Binary {
              D.op = "+"
            , D.left = D.Binary {
                D.op = "*"
              , D.left = D.Identifire {
                  D.name = "hoge"
                }
              , D.right = D.Literal {
                  D.value = "2"
                }
              }
            , D.right = D.Binary {
                D.op = "/"
              , D.left = D.Identifire {
                  D.name = "fuga"
                }
              , D.right = D.Binary {
                  D.op = "-"
                , D.left = D.Literal {
                    D.value = "4"
                  }
                , D.right = D.Literal {
                    D.value = "5"
                  }
                }
              }
            }



  ]

--------------------------------------------------------------
-- | testProgram
--------------------------------------------------------------
--
testProgramInput = [r|
#include <stdio.h>
#include    "../../external_inc.h"

#define HOGE    (1)
#define FUGA    (2)
#define VARI    HOGE


#if VARI == HOGE
char hoge_globvar = 100;
  #if VARI_2 == HOGE
static int hoge_globvar_2 = xxx;
  #endif
#endif    /* VARI */

int arr_var[10];

int main( void )
{
    int local_var;
    local_var = 0;
    local_var++;

    printf("local_var...%d\n", local_var);

    return (0);
}
|]

testProgram :: Test
testProgram = TestList
  [ "testProgram normal 1" ~:
        (exRes $ stParse [] program testProgramInput `feed` "") ~?= Right
            (
              D.Ast [
                D.Preprocess {
                  D.contents = D.Include {
                    D.prepro = []
                  , D.file = "<stdio.h>"
                  }
                }

              , D.Preprocess {
                  D.contents = D.Include {
                    D.prepro = []
                  , D.file = "../../external_inc.h"
                  }
                }

              , D.Preprocess {
                  D.contents = D.Define {
                    D.prepro = []
                  , D.name = "HOGE"
                  , D.value = "(1)"
                  }
                }

              , D.Preprocess {
                  D.contents = D.Define {
                    D.prepro = []
                  , D.name = "FUGA"
                  , D.value = "(2)"
                  }
                }

              , D.Preprocess {
                  D.contents = D.Define {
                    D.prepro = []
                  , D.name = "VARI"
                  , D.value = "HOGE"
                  }
                }

              , D.Var {
                  D.prepro = [
                    D.Condition {
                      D.command = "#if"
                    , D.expr = D.Binary {
                        D.op = "=="
                      , D.left = D.Identifire {
                          D.name = "VARI"
                        }
                      , D.right = D.Identifire {
                          D.name = "HOGE"
                        }
                      }
                    }
                  ]
                , D.typ = ["char"]
                , D.name = "hoge_globvar"
                , D.initVal = Just (D.Literal {
                    D.value = "100"
                  })
                }

              , D.Var {
                  D.prepro = [
                    D.Condition {
                      D.command = "#if"
                    , D.expr = D.Binary {
                        D.op = "=="
                      , D.left = D.Identifire {
                          D.name = "VARI"
                        }
                      , D.right = D.Identifire {
                          D.name = "HOGE"
                        }
                      }
                    }
                  , D.Condition {
                      D.command = "#if"
                    , D.expr = D.Binary {
                        D.op = "=="
                      , D.left = D.Identifire {
                          D.name = "VARI_2"
                        }
                      , D.right = D.Identifire {
                          D.name = "HOGE"
                        }
                      }
                    }
                  ]
                , D.typ = ["static", "int"]
                , D.name = "hoge_globvar_2"
                , D.initVal = Just (D.Identifire {
                    D.name = "xxx"
                  })
                }

              , D.Var {
                  D.prepro = []
                , D.typ = ["int", "[10]"]
                , D.name = "arr_var"
                , D.initVal = Nothing
                }

              , D.Func {
                  D.prepro = []
                , D.return = ["int"]
                , D.name = "main"
                , D.args = [
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
                  , D.ExpState {
                      D.prepro = []
                    , D.contents = D.Binary {
                        D.op = "="
                      , D.left = D.Identifire {
                          D.name = "local_var"
                        }
                      , D.right = D.Literal {
                          D.value = "0"
                        }
                      }
                    }
                  , D.ExpState {
                      D.prepro = []
                    , D.contents = D.PostUnary {
                        D.op = "++"
                      , D.operand = D.Identifire {
                          D.name = "local_var"
                        }
                      }
                    }
                  , D.ExpState {
                      D.prepro = []
                    , D.contents = D.Call {
                        D.func = "printf"
                      , D.args = [
                          D.StrLiteral {
                            D.value = "local_var...%d\\n"
                          }
                        , D.Identifire {
                            D.name = "local_var"
                          }
                        ]
                      }
                    }
                  , D.Return {
                      D.prepro = []
                    , D.operand = D.Literal {
                        D.value = "0"
                      }
                    }
                  ]
                }
              ]
            )

  ]



