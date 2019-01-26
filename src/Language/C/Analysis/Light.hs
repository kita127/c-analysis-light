{-# LANGUAGE OverloadedStrings #-}
module Language.C.Analysis.Light
( analyze
) where

import qualified Data.Text as T

data C = Prepro T.Text C
       | Csrc Cstate C
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



analyze :: T.Text -> T.Text
analyze = id
