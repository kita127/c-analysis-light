{-# LANGUAGE OverloadedStrings #-}
import           Data.Attoparsec.Text      hiding (take)
import qualified Data.Text                 as T
import           Language.C.Analysis.Light
import           Test.HUnit

exRes :: Result a -> Either String a
exRes (Done _ r)   = Right r
exRes (Fail _ _ s) = Left s
exRes (Partial _)  = Left "Partial"

main :: IO ()
main = do
    runTestTT $ TestList
      [ testSample
      , testDefVariable
      ]
    return ()


testSample :: Test
testSample = TestList
  [ "testSample test 1" ~:
        "hello test" ~?= "hello test"
  ]

testDefVariable :: Test
testDefVariable = TestList
  [ "testDefVariable normal 1" ~:
        (exRes $ parse defVariable "int hoge;") ~?= Right
            Var {
              typ = "int"
            , name = "hoge"
            , initVal = Nothing
            }
  ]
