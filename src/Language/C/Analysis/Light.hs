{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.C.Analysis.Light
( analyze
, token
--, statement
, defVariable
--, defFunction
, identifire
, value
--, arguments
--, preprocess
--, preproIfStart
) where

import           Control.Applicative
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State
import           Data.Attoparsec.Text           hiding (take)
import           Data.Functor                   (($>))
import           Data.List                      (intercalate)
import           Data.Monoid                    (mempty)
import qualified Data.Text                      as T
import qualified Language.C.Analysis.Light.Data as DATA
-- import           Data.Aeson.TH

type SParser a = StateT DATA.Condition Parser a

type IDStr = T.Text
type TypeStr = T.Text


-- | analyze
--
analyze :: T.Text -> Either String DATA.C
analyze s = case parse (p <* endOfInput) s `feed` "" of
    (Done _ r)    -> Right r
    (Fail i ss w) -> Left $ intercalate " : " ((show i):w:ss)
    (Partial _)   -> Left "partial ..."
    where
        p = (`evalStateT` []) cLang

-- | cLang
--
-- TODO:
-- pre の型をわかりやす名前に置き換えたい type PRE など
--
cLang :: SParser DATA.C
cLang = statement <|> pure DATA.End
--cLang pre = preproIfStart <|> preprocess pre <|> statement pre <|> preproIfEnd <|> pure DATA.End


-- | token
--
token :: SParser a -> SParser a
token p = spaceOrComment *> p <* spaceOrComment

-- | spaceOrComment
--
spaceOrComment :: SParser ()
spaceOrComment = skipMany $
    lift space $> () <|> comment1 <|> comment2

-- | comment1
--
comment1 :: SParser ()
comment1 = do
    lift $ string "/*"
    lift consume

    where
        consume = string "*/" $> () <|> anyChar *> consume

-- | comment2
--
-- // ~~~~~~~~~~~~~
--
comment2 :: SParser ()
comment2 = lift $ string "//" *> takeTill isEndOfLine *> endOfLine



-- | statement
--
statement :: SParser DATA.C
statement = do
        ps <- get
        DATA.Csrc <$> pure ps <*> defVariable <*> cLang
    -- <|> DATA.Csrc <$> pure pre <*> defFunction pre <*> cLang pre


-- | defVariable
--
defVariable :: SParser DATA.Cstate
defVariable = do
    (name, types) <- typeAndID
    v <- initValue
    token $ lift $ char ';'
    return $ DATA.Var types name v


-- | typeAndID
--
typeAndID :: SParser (IDStr, [TypeStr])
typeAndID = do
    ids <- many1 $ identifire <|> pointer
    return (last ids, init ids)


-- | identifire
--
identifire :: SParser T.Text
identifire = token $ do
    head' <- lift $ letter <|> char '_'
    tail' <- lift $ many1 idLetter
    return $ T.pack $ head' : tail'


-- | idLetter
--
idLetter :: Parser Char
idLetter = letter <|> digit <|> char '_'



-- | pointer
--
pointer :: SParser T.Text
pointer = token $ lift $ string "*"



-- | initValue
--
initValue :: SParser (Maybe T.Text)
initValue = Just <$> p <|> pure Nothing
    where
        p :: SParser T.Text
        p = token (lift $ char '=') *> value


-- | value
--
value :: SParser T.Text
value = token $ hex <|> lift (T.pack <$> many1 digit) <|> addressVal <|> identifire
    where
        addressVal = do
            lift $ char '&'
            n <- identifire
            return $ '&' `T.cons` n

-- | hex
--
hex :: SParser T.Text
hex = do
    lift $ string "0x"
    n <- p
    return $ T.pack ('0':'x':n)
    where
        p :: SParser String
        p = lift $ many1 $ satisfy $ inClass "a-fA-F0-9"




---- | preprocess
----
--preprocess :: [T.Text] -> Parser DATA.C
--preprocess pre = DATA.Prepro <$> pure pre <*> include <*> cLang pre
--
--
---- | include
----
--include :: Parser DATA.PreState
--include =  DATA.Include <$ token (string "#include") <*> file <* tillEndOfLine
--
---- | file
----
--file :: Parser T.Text
--file = string "<" `liftAp` identifire `liftAp` string ".h" `liftAp` string ">"
--
--
---- | tillEndOfLine
----
--tillEndOfLine :: Parser ()
--tillEndOfLine = takeTill isEndOfLine *> endOfLine
--
--


---- | preproIfStart
----
--preproIfStart :: Parser DATA.C
--preproIfStart = do
--    skipMany space
--    s <- p
--    d <- statement $ pure s
--    return d
--    where
--        p = preIfState
--
---- | preIfState
----
--preIfState :: Parser T.Text
--preIfState = do
--    preCond <- token $ string "#if"
--    left    <- token identifire
--    ex      <- token $ string "=="
--    right   <- value
--    tillEndOfLine
--    return $ T.intercalate " " [preCond, left, ex, right]
--
---- | preproIfEnd
----
--preproIfEnd :: Parser DATA.C
--preproIfEnd = do
--    token $ string "#endif"
--    cLang mempty
--
--
---- | defFunction
----
--defFunction :: [T.Text] -> Parser DATA.Cstate
--defFunction pre = do
--    (name, ret) <- typeAndID
--    token $ char '('
--    args <- arguments
--    token $ char ')'
--    p <- block pre
--    return $ DATA.Func ret name args p
--
---- | block
----
--block :: [T.Text] -> Parser [DATA.Proc]
--block pre = do
--    token $ char '{'
--    ps <- many' $ process pre
--    token $ char '}'
--    return ps
--
---- | process
----
--process :: [T.Text] -> Parser DATA.Proc
--process pre = callFunc pre <|> funcReturn pre
--
---- | funcReturn
----
--funcReturn :: [T.Text] -> Parser DATA.Proc
--funcReturn pre = do
--    token $ string "return"
--    token $ char '('
--    v <- token $ value
--    token $ char ')'
--    token $ char ';'
--    return $ DATA.Return pre v
--
---- | callFunc
----
--callFunc :: [T.Text] -> Parser DATA.Proc
--callFunc pre = do
--    f <- token $ identifire
--    token $ char '('
--    a <- strLiteral
--    token $ char ')'
--    token $ char ';'
--    return $ DATA.Call pre f [a]
--
--
--
---- | strLiteral
----
--strLiteral :: Parser T.Text
--strLiteral = token $ do
--    string "\"" `liftAp` takeTill (== '"') `liftAp` "\""
--
---- | arguments
----
--arguments :: Parser [DATA.Cstate]
--arguments = void <|> justArgs
--    where
--        void = token (string "void") $>
--            [DATA.Var {DATA.typ = ["void"], DATA.name = "", DATA.initVal = Nothing}]
--
--        justArgs = (flip sepBy1) (char ',') $ do
--            (name, types) <- typeAndID
--            return $ DATA.Var types name Nothing
--
--
--
--
--
--
--
---- | liftAp
----
---- T.append の リフト関数
----
--liftAp :: Parser T.Text -> Parser T.Text -> Parser T.Text
--liftAp = liftA2 T.append
