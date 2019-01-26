module Main where

import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Language.C.Analysis.Light (analyze)
import           Options.Applicative



-----------------------------------------------------------------------
-- コマンドラインオプション
-----------------------------------------------------------------------
data Option = Option
    { args :: [String]
    } deriving (Eq, Show)

myOpt :: Parser Option
myOpt = Option
    <$> argOption
--  <*> hogeOption
    where
        argOption :: Parser [String]
        argOption = some $ strArgument
            $ help "input files"
            <> metavar "FILE"
            <> action "file"        -- bash に補完をおまかせする

parserInfo :: ParserInfo Option
parserInfo = info (helper <*> myOpt)
    $  fullDesc
    <> progDesc "descliption"
    <> header "header"

-----------------------------------------------------------------------


main :: IO ()
main = do
    -- 入力
    options <- execParser parserInfo
    contents <- TIO.readFile . head . args $ options

    let d = analyze contents

    print d
