{-# LANGUAGE OverloadedStrings #-}
module Language.C.Analysis.Light
( C(..)
, Cstate(..)
, analyze
, statement
, defVariable
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
analyze s = case parse (cLang) s `feed` "" of
--analyze s = case parse (cLang <* endOfInput) s `feed` "" of
    (Done _ r) -> Right r
    a          -> Left "error"

--  cLang
--
cLang :: Parser C
cLang = statement <|> pure End


-- | statement
--
statement :: Parser C
statement = Csrc <$> defVariable <*> cLang

-- | defVariable
--
defVariable :: Parser Cstate
defVariable = Var <$> string "int" <* space <*> string "hoge" <* char ';' <*> pure Nothing
