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

-- | convList
--
-- TODO:
-- Json に End のゴミが残るの直したい
--
convList :: DATA.C -> [DATA.C]
convList DATA.End = []
convList c        = c{DATA.next = End} : convList (DATA.next c)


-- | encodeJson
--
encodeJson :: DATA.C -> B.ByteString
encodeJson = PA.encodePretty . convList
