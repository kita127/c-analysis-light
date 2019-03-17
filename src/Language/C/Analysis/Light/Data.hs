{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.C.Analysis.Light.Data
( Statement(..)
, Ast(..)
, PreState(..)
, Cstate(..)
, Proc(..)
, Condition(..)
, Exp(..)
) where

import           Data.Aeson.TH
import           Data.Attoparsec.Text hiding (take)
import qualified Data.Text            as T

data Condition = Condition
                 { command :: T.Text
                 , expr    :: Exp
                 }
                 deriving (Eq, Show)

data Ast = Ast [Statement]

data Statement = Preprocess
                  { contents :: PreState
                  }
                | Csrc
                  { statements :: Cstate
                  }
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
              , initVal :: Maybe Exp
              }
            | Func
              { prepro :: [Condition]
              , return :: [T.Text]
              , name   :: T.Text
              , args   :: [Cstate]
              , procs  :: [Proc]
              }
            deriving (Eq, Show)

-- TODO:
-- Proc -> State とかにしたい
--
data Proc = Return
            { prepro  :: [Condition]
            , operand :: Exp
            }
          | LVar
            { prepro :: [Condition]
            , var    :: Cstate
            }
          | ExpState
            { prepro   :: [Condition]
            , contents :: Exp
            }
          deriving (Eq, Show)

data Exp = PreUnary
           { op      :: T.Text
           , operand :: Exp
           }
         | Binary
           { op    :: T.Text
           , left  :: Exp
           , right :: Exp
           }
         | Call
           { func :: T.Text
           , args :: [Exp]
           }
         | Identifire
           { name :: T.Text }
         | Literal
           { value :: T.Text }
         | StrLiteral
           { value :: T.Text }
          deriving (Eq, Show)


-- TemplateHaskell
deriveJSON defaultOptions ''Condition
deriveJSON defaultOptions ''Ast
deriveJSON defaultOptions ''Statement
deriveJSON defaultOptions ''PreState
deriveJSON defaultOptions ''Cstate
deriveJSON defaultOptions ''Proc
deriveJSON defaultOptions ''Exp

