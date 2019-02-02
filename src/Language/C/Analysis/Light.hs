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
) where

import           Control.Applicative
import           Data.Aeson.TH
import           Data.Attoparsec.Text           hiding (take)
import           Data.Functor                   (($>))
import qualified Data.Text                      as T
import qualified Language.C.Analysis.Light.Data as DATA

type IDStr = T.Text
type TypeStr = T.Text

-- | analyze
--
analyze :: T.Text -> Either String DATA.C
analyze s = case parse (cLang <* endOfInput) s `feed` "" of
    (Done _ r) -> Right r
    _          -> Left "error"

--  cLang
--
cLang :: Parser DATA.C
cLang = statement <|> pure DATA.End

-- | token
--
token :: Parser a -> Parser a
token p = many' space *> p <* many' space

-- | blanks
--
-- 1つ以上のスペース
--
blanks :: Parser ()
blanks = many1 space $> ()

-- | statement
--
statement :: Parser DATA.C
statement =
        DATA.Csrc <$> defVariable <*> cLang
    <|> DATA.Csrc <$> defFunction <*> cLang

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
arguments = void <|> justArg
    where
        void = token (string "void") $>
            [DATA.Var {DATA.typ = ["void"], DATA.name = "", DATA.initVal = Nothing}]

        justArg = many1 $ do
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
defVariable = token $ do
    (name, types) <- typeAndID
    v <- initValue
    char ';'
    return $ DATA.Var types name v

-- | initValue
--
initValue :: Parser (Maybe T.Text)
initValue = token $ (Just <$> p) <|> pure Nothing
    where
        p :: Parser T.Text
        p = char '=' *> blanks *> value

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
