{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.C.Analysis.Light.Data
( C(..)
, PreState(..)
, Cstate(..)
, Proc(..)
, Condition(..)
, Exp(..)
, Operation(..)
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
            , args   :: [Exp]
            }
          | Return
            { prepro :: [Condition]
            , value  :: T.Text
            }
          | LVar
            { prepro :: [Condition]
            , var    :: Cstate
            }
          | Assigne
            { prepro :: [Condition]
            , left   :: T.Text
            , right  :: Exp
            }
          | Exprssions
            { prepro   :: [Condition]
            , contents :: Exp
            }
          deriving (Eq, Show)

-- TODO:
-- op をメンバの始めに持って
--
-- Add とか Sub とかわかりづらいのでやめるかも
--
data Exp = Binary
           { left  :: Exp
           , op    :: Operation
           , right :: Exp
           }
         | Identifire
           { id :: T.Text }
         | Literal
           { value :: T.Text }
         | StrLiteral
           { str :: T.Text }
          deriving (Eq, Show)

data Operation = Add | Sub | Mul | Div
          deriving (Eq, Show)

-- TemplateHaskell
deriveJSON defaultOptions ''Condition
deriveJSON defaultOptions ''C
deriveJSON defaultOptions ''PreState
deriveJSON defaultOptions ''Cstate
deriveJSON defaultOptions ''Proc
deriveJSON defaultOptions ''Exp
deriveJSON defaultOptions ''Operation

