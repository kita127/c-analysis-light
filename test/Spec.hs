{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text  as T
import           Test.HUnit

main :: IO ()
main = do
    runTestTT $ TestList
      [ testSample
      ]
    return ()


testSample :: Test
testSample = TestList
  [ "testSample test 1" ~:
        "hello test" ~?= "hello test"
  ]
