{-# LANGUAGE OverloadedStrings #-}
module Language.C.Analysis.Light.Encode
( encodeJson
) where

import           Data.Aeson.Encode.Pretty       as PA
import qualified Data.ByteString.Lazy.Char8     as B
import           Language.C.Analysis.Light.Data as DATA
-- import qualified Data.Text                  as T
-- import           Data.Aeson.TH
-- import           Data.Aeson



-- | encodeJson
--
encodeJson :: DATA.Ast -> B.ByteString
encodeJson = PA.encodePretty
