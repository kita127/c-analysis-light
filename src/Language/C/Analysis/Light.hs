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
import           Data.Functor         (($>))
import qualified Data.Text            as T

data C = Prepro T.Text C
       | Csrc Cstate C
       | End
    deriving (Eq, Show)


data Cstate = Var
              { typ     :: [T.Text]
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
    _          -> Left "error"

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
blanks = many1 space $> ()

-- | statement
--
statement :: Parser C
statement = Csrc <$> defVariable <*> cLang

-- | defVariable
--
defVariable :: Parser Cstate
defVariable = token $ do
    ids <- many1 $ token $ identifire <|> pointer
    v <- initValue
    char ';'
    let (n, ts) = (last ids, init ids)
    return $ Var ts n v

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
