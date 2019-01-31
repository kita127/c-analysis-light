module Main where

import qualified Data.ByteString.Lazy.Char8       as B
import qualified Data.Text.IO                     as TIO
import           Language.C.Analysis.Light        (analyze)
import           Language.C.Analysis.Light.Encode (encodeJson)
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

    let d = case analyze contents of
                (Right r) -> r
                (Left s)  -> error s

    let jd = encodeJson d

    B.putStrLn jd
