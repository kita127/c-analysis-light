{-# LANGUAGE OverloadedStrings #-}
module Language.C.Analysis.Light
( analyze
) where

import qualified Data.Text as T


analyze :: T.Text -> T.Text
analyze = id
