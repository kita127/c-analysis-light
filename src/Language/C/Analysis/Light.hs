{-# LANGUAGE OverloadedStrings #-}
module Language.C.Analysis.Light
( C(..)
, Cstate(..)
, token
, analyze
, statement
, defVariable
, identifire
, value
) where

import           Control.Applicative
import           Data.Attoparsec.Text hiding (take)
import qualified Data.Text            as T

data C = Prepro T.Text C
       | Csrc Cstate C
       | End
    deriving (Eq, Show)


data Cstate = Var
              { typ     :: T.Text
              , name    :: T.Text
              , initVal :: Maybe T.Text
              }
            | Func
              { name    :: T.Text
              }
            deriving (Eq, Show)


-- | analyze
--
analyze :: T.Text -> Either String C
analyze s = case parse (cLang <* endOfInput) s `feed` "" of
    (Done _ r) -> Right r
    a          -> Left "error"

--  cLang
--
cLang :: Parser C
cLang = statement <|> pure End

-- | token
--
token :: Parser a -> Parser a
token p = many' space *> p <* many' space

-- | blanks
--
-- 1つ以上のスペース
--
blanks :: Parser ()
blanks = many1 space *> pure ()

-- | statement
--
statement :: Parser C
statement = Csrc <$> defVariable <*> cLang

-- | defVariable
--
defVariable :: Parser Cstate
defVariable = token $
    Var <$> (T.pack <$> many1 letter) <* blanks <*> identifire <*> initValue <* char ';'

-- | initValue
--
initValue :: Parser (Maybe T.Text)
initValue = token $ (Just <$> p) <|> pure Nothing
    where
        p :: Parser T.Text
        p = char '=' *> blanks *> value

-- | identifire
--
identifire :: Parser T.Text
identifire = do
    head' <- letter <|> char '_'
    tail' <- many1 idLetter
    return $ T.pack $ head' : tail'

-- | value
--
value :: Parser T.Text
value = hex <|> (T.pack <$> many1 digit) <|> identifire

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
