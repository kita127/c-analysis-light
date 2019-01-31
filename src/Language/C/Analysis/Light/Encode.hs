{-# LANGUAGE OverloadedStrings #-}
module Language.C.Analysis.Light.Encode
( encodeJson
) where

import           Data.Aeson.Encode.Pretty   as PA
import qualified Data.ByteString.Lazy.Char8 as B
import           Language.C.Analysis.Light
-- import qualified Data.Text                  as T
-- import           Data.Aeson.TH
-- import           Data.Aeson

-- | convList
--
-- TODO:
-- Json に End のゴミが残るの直したい
--
convList :: C -> [C]
convList End = []
convList c   = c{next = End} : convList (next c)


-- | encodeJson
--
encodeJson :: C -> B.ByteString
encodeJson = PA.encodePretty . convList
