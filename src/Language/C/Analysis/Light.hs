{-# LANGUAGE OverloadedStrings #-}
module Language.C.Analysis.Light
( analyze
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
--cLang = defVariable <|> function <|> other <|> pure DataEnd


statement :: Parser C
statement = undefined
