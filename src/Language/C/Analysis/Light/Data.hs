{-# LANGUAGE TemplateHaskell #-}
module Language.C.Analysis.Light.Data
( C(..)
, Cstate(..)
) where

import           Data.Aeson.TH
import           Data.Attoparsec.Text hiding (take)
import qualified Data.Text            as T



data C = Prepro
         { prepro   :: Maybe T.Text
         , contents :: T.Text
         , next     :: C
         }
       | Csrc
         { prepro     :: Maybe T.Text
         , statements :: Cstate
         , next       :: C
         }
       | End
    deriving (Eq, Show)


data Cstate = Var
              { typ     :: [T.Text]
              , name    :: T.Text
              , initVal :: Maybe T.Text
              }
            | Func
              { return :: [T.Text]
              , name   :: T.Text
              , args   :: [Cstate]
              }
            deriving (Eq, Show)

-- TemplateHaskell
deriveJSON defaultOptions ''C
deriveJSON defaultOptions ''Cstate

