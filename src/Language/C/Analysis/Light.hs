{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Language.C.Analysis.Light
( token
, analyze
, statement
, defVariable
, defFunction
, identifire
, value
, arguments
, preproIfStart
) where

import           Control.Applicative
import           Data.Aeson.TH
import           Data.Attoparsec.Text           hiding (take)
import           Data.Functor                   (($>))
import           Data.List                      (intercalate)
import qualified Data.Text                      as T
import qualified Language.C.Analysis.Light.Data as DATA

type IDStr = T.Text
type TypeStr = T.Text

-- | analyze
--
analyze :: T.Text -> Either String DATA.C
analyze s = case parse (cParseStart <* endOfInput) s `feed` "" of
    (Done _ r)    -> Right r
    (Fail i ss s) -> Left $ intercalate " : " ((show i):s:ss)
    (Partial _)   -> Left "partial ..."
    where
        cParseStart = cLang Nothing

--  cLang
--
cLang :: Maybe T.Text -> Parser DATA.C
cLang pre = preproIfStart <|> statement pre <|> preproIfEnd <|> pure DATA.End

-- | token
--
token :: Parser a -> Parser a
token p = spaceOrComment *> p <* spaceOrComment

-- | spaceOrComment
--
spaceOrComment :: Parser ()
spaceOrComment = skipMany $
    space $> () <|> comment1 <|> comment2

-- | comment1
--
comment1 :: Parser ()
comment1 = do
    string "/*"
    consume

    where
        consume = string "*/" $> () <|> anyChar *> consume

-- | comment2
--
-- // ~~~~~~~~~~~~~
--
comment2 :: Parser ()
comment2 = string "//" *> takeTill isEndOfLine *> endOfLine

-- | statement
--
statement :: Maybe T.Text -> Parser DATA.C
statement pre =
        DATA.Csrc <$> pure pre <*> defVariable <*> cLang pre
    <|> DATA.Csrc <$> pure pre <*> defFunction <*> cLang pre

-- | preproIfStart
--
preproIfStart :: Parser DATA.C
preproIfStart = do
    skipMany space
    s <- string "#if PRE_VARI == 1"
    endOfLine
    d <- statement (Just s)
    return d

-- | preproIfEnd
--
preproIfEnd :: Parser DATA.C
preproIfEnd = do
    endOfLine
    string "#endif"
    cLang Nothing


-- | defFunction
--
defFunction :: Parser DATA.Cstate
defFunction = do
    (name, ret) <- typeAndID
    token $ char '('
    args <- arguments
    token $ char ')'
    token $ char '{'
    token $ char '}'
    return $ DATA.Func ret name args

-- | arguments
--
arguments :: Parser [DATA.Cstate]
arguments = void <|> justArgs
    where
        void = token (string "void") $>
            [DATA.Var {DATA.typ = ["void"], DATA.name = "", DATA.initVal = Nothing}]

        justArgs = (flip sepBy1) (char ',') $ do
            (name, types) <- typeAndID
            return $ DATA.Var types name Nothing

-- | typeAndID
--
typeAndID :: Parser (IDStr, [TypeStr])
typeAndID = do
    ids <- many1 $ token $ identifire <|> pointer
    return (last ids, init ids)

-- | defVariable
--
defVariable :: Parser DATA.Cstate
defVariable = do
    (name, types) <- typeAndID
    v <- initValue
    char ';'
    return $ DATA.Var types name v

-- | initValue
--
initValue :: Parser (Maybe T.Text)
initValue = Just <$> p <|> pure Nothing
    where
        p :: Parser T.Text
        p = token (char '=') *> token value

-- | identifire
--
-- TODO:
identifire :: Parser T.Text
identifire = do
    head' <- letter <|> char '_'
    tail' <- many1 idLetter
    return $ T.pack $ head' : tail'

-- | pointer
--
pointer :: Parser T.Text
pointer = string "*"

-- | value
--
value :: Parser T.Text
value = hex <|> (T.pack <$> many1 digit) <|> addressVal <|> identifire
    where
        addressVal = do
            char '&'
            n <- identifire
            return $ '&' `T.cons` n

-- | hex
--
hex :: Parser T.Text
hex = do
    string "0x"
    n <- p
    return $ T.pack ('0':'x':n)
    where
        p :: Parser String
        p = many1 $ satisfy $ inClass "a-fA-F0-9"

-- | idLetter
--
idLetter :: Parser Char
idLetter = letter <|> digit <|> char '_'
