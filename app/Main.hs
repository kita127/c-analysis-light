module Main where

import qualified Data.Text                 as T
import           Language.C.Analysis.Light (analyze)

main :: IO ()
main = do
    -- 入力

    let d = analyze $ T.pack "input"

    print d
