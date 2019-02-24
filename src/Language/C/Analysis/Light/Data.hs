{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.C.Analysis.Light.Data
( C(..)
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

-- TODO:
-- Exprssions を式文とかにしたい
--
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

data Exp = Binary
           { op    :: T.Text
           , left  :: Exp
           , right :: Exp
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
deriveJSON defaultOptions ''C
deriveJSON defaultOptions ''PreState
deriveJSON defaultOptions ''Cstate
deriveJSON defaultOptions ''Proc
deriveJSON defaultOptions ''Exp

