{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.C.Analysis.Light.Data
( C(..)
, PreState(..)
, Cstate(..)
, Proc(..)
) where

import           Data.Aeson.TH
import           Data.Attoparsec.Text hiding (take)
import qualified Data.Text            as T



data C = Prepro
         { prepro   :: Maybe T.Text
         , contents :: PreState
         , next     :: C
         }
       | Csrc
         { prepro     :: Maybe T.Text
         , statements :: Cstate
         , next       :: C
         }
       | End
    deriving (Eq, Show)

data PreState = Include { file :: T.Text } deriving (Eq, Show)

-- TODO:
-- typ -> type にしたい
--
data Cstate = Var
              { typ     :: [T.Text]
              , name    :: T.Text
              , initVal :: Maybe T.Text
              }
            | Func
              { return :: [T.Text]
              , name   :: T.Text
              , args   :: [Cstate]
              , procs  :: [Proc]
              }
            deriving (Eq, Show)

data Proc = Call
            { name :: T.Text
            , args :: [T.Text]
            }
          | Return
            { value :: T.Text
            }
          deriving (Eq, Show)

-- TemplateHaskell
deriveJSON defaultOptions ''C
deriveJSON defaultOptions ''PreState
deriveJSON defaultOptions ''Cstate
deriveJSON defaultOptions ''Proc

