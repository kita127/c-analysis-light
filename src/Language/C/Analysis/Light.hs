{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.C.Analysis.Light
( analyze
, token
--, statement
, defVariable
, defFunction
, identifire
, value
--, arguments
--, preprocess
--, preproIfStart
) where

import           Control.Applicative
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State      (StateT, evalStateT, get,
                                                 modify)
import           Data.Attoparsec.Text           hiding (take)
import           Data.Functor                   (($>))
import           Data.List                      (intercalate)
import           Data.Monoid                    (mempty)
import qualified Data.Text                      as T
import qualified Language.C.Analysis.Light.Data as DATA
-- import           Data.Aeson.TH

type SParser a = StateT [DATA.Condition] Parser a

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


-- | update
--
-- TODO:
-- プリプロ最後改行きたかでパースしてないので対応する
--
update :: SParser a -> SParser a
update p = do
    -- Start Check
    ss <- many' condStart
    modify (++ ss)

    r <- p

    -- End Check
    es <- many' condEnd
    modify (take (length ss - length es))
    return r

-- | condStart
--
condStart :: SParser DATA.Condition
condStart = do
    c <- token $ lift $ string "#if"
    l <- identifire
    o <- token $ lift $ string "=="
    r <- value
    return $ DATA.Condition c l o r

-- | condEnd
--
condEnd :: SParser ()
condEnd = token $ lift $ string "#endif" *> pure ()



-- | statement
--
statement :: SParser DATA.C
statement =
        DATA.Csrc <$> defVariable <*> cLang
    <|> DATA.Csrc <$> defFunction <*> cLang


-- | defVariable
--
defVariable :: SParser DATA.Cstate
defVariable = update $ do
    (name, types) <- typeAndID
    v <- initValue
    token $ lift $ char ';'
    s <- get
    return $ DATA.Var s types name v


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


-- | defFunction
--
defFunction :: SParser DATA.Cstate
defFunction = do
    (name, ret) <- typeAndID
    token $ lift $ char '('
    args <- arguments
    token $ lift $ char ')'
    p <- block
    s <- get
    return $ DATA.Func s ret name args p


-- | arguments
--
arguments :: SParser [DATA.Cstate]
arguments = void <|> justArgs
    where
        void :: SParser [DATA.Cstate]
        void = do
            token $ lift $ string "void"
            s <- get
            return $ [ DATA.Var s ["void"] "" Nothing ]

-- | justArgs
--
justArgs :: SParser [DATA.Cstate]
justArgs = (`sepBy1` comma) $ do
    (name, types) <- typeAndID
    s <- get
    return $ DATA.Var s types name Nothing
    where
        comma = lift $ char ','


-- | block
--
block :: SParser [DATA.Proc]
block = do
    token $ lift $ char '{'
    ps <- many' $ process
    token $ lift $ char '}'
    return ps


-- | process
--
process :: SParser DATA.Proc
process = funcReturn <|> callFunc

-- | funcReturn
--
funcReturn :: SParser DATA.Proc
funcReturn = update $ do
    token $ lift $ string "return"
    token $ lift $ char '('
    v <- token $ value
    token $ lift $ char ')'
    token $ lift $ char ';'
    s <- get
    return $ DATA.Return s v


-- | callFunc
--
callFunc :: SParser DATA.Proc
callFunc = update $ do
    f <- token $ identifire
    token $ lift $ char '('
    a <- strLiteral
    token $ lift $ char ')'
    token $ lift $ char ';'
    s <- get
    return $ DATA.Call s f [a]

-- | strLiteral
--
strLiteral :: SParser T.Text
strLiteral = token $ lift $ do
    string "\"" `liftAp` takeTill (== '"') `liftAp` "\""


-- | liftAp
--
-- T.append の リフト関数
--
liftAp :: Parser T.Text -> Parser T.Text -> Parser T.Text
liftAp = liftA2 T.append



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
--
--
