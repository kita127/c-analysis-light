{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.C.Analysis.Light.Data
( C(..)
, PreState(..)
, Cstate(..)
, Proc(..)
, Condition(..)
) where

import           Data.Aeson.TH
import           Data.Attoparsec.Text hiding (take)
import qualified Data.Text            as T

data Condition = Condition
                 { command :: T.Text
                 , left    :: T.Text
                 , op      :: T.Text
                 , right   :: T.Text
                 }
                 deriving (Eq, Show)


data C = Prepro
         { contents :: PreState
         , next     :: C
         }
       | Csrc
         { statements :: Cstate
         , next       :: C
         }
       | End
    deriving (Eq, Show)

data PreState = Include
                { prepro :: [Condition]
                , file   :: T.Text
                }
              deriving (Eq, Show)

-- TODO:
-- typ -> type にしたい
--
data Cstate = Var
              { prepro  :: [Condition]
              , typ     :: [T.Text]
              , name    :: T.Text
              , initVal :: Maybe T.Text
              }
            | Func
              { prepro :: [Condition]
              , return :: [T.Text]
              , name   :: T.Text
              , args   :: [Cstate]
              , procs  :: [Proc]
              }
            deriving (Eq, Show)

data Proc = Call
            { prepro :: [Condition]
            , name   :: T.Text
            , args   :: [T.Text]
            }
          | Return
            { prepro :: [Condition]
            , value  :: T.Text
            }
          | LVar
            { prepro  :: [Condition]
            , var     :: Cstate
            }
          deriving (Eq, Show)

-- TemplateHaskell
deriveJSON defaultOptions ''Condition
deriveJSON defaultOptions ''C
deriveJSON defaultOptions ''PreState
deriveJSON defaultOptions ''Cstate
deriveJSON defaultOptions ''Proc

