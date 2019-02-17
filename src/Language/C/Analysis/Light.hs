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
, include
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
import qualified Language.C.Analysis.Light.Data as D
-- import           Data.Aeson.TH

type SParser a = StateT [D.Condition] Parser a

type IDStr = T.Text
type TypeStr = T.Text


-- | analyze
--
analyze :: T.Text -> Either String D.C
analyze s = case parse (p <* endOfInput) s `feed` "" of
    (Done _ r)    -> Right r
    (Fail i ss w) -> Left $ intercalate " : " ((show i):w:ss)
    (Partial _)   -> Left "partial ..."
    where
        p = (`evalStateT` []) cLang

-- | cLang
--
cLang :: SParser D.C
cLang = preprocess <|> statement <|> pure D.End
--cLang pre = preproIfStart <|> preprocess pre <|> statement pre <|> preproIfEnd <|> pure D.End

-- | preprocess
--
preprocess :: SParser D.C
preprocess = D.Prepro <$> include <*> cLang


-- | include
--
include :: SParser D.PreState
include =  D.Include <$> get <* token (lift $ string "#include") <*> file <* tillEndOfLine

-- | file
--
file :: SParser T.Text
file = lift (string "<") `liftAp` identifire `liftAp` lift (string ".h") `liftAp` lift (string ">")


-- | tillEndOfLine
--
tillEndOfLine :: SParser ()
tillEndOfLine = lift $ takeTill isEndOfLine *> endOfLine




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
condStart :: SParser D.Condition
condStart = do
    c <- token $ lift $ string "#if"
    l <- identifire
    o <- token $ lift $ string "=="
    r <- value
    return $ D.Condition c l o r

-- | condEnd
--
condEnd :: SParser ()
condEnd = token $ lift $ string "#endif" *> pure ()



-- | statement
--
statement :: SParser D.C
statement =
        D.Csrc <$> defVariable <*> cLang
    <|> D.Csrc <$> defFunction <*> cLang


-- | defVariable
--
defVariable :: SParser D.Cstate
defVariable = update $ do
    (name, types) <- typeAndID
    v <- initValue
    semicolon
    s <- get
    return $ D.Var s types name v


-- | typeAndID
--
typeAndID :: SParser (IDStr, [TypeStr])
typeAndID = do
    t <- identifire <|> pointer
    ts <- many1 $ identifire <|> pointer
    let ids = t:ts
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
        p = equal *> value


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
defFunction :: SParser D.Cstate
defFunction = do
    (name, ret) <- typeAndID
    sParen
    args <- arguments
    eParen
    p <- block
    s <- get
    return $ D.Func s ret name args p


-- | arguments
--
arguments :: SParser [D.Cstate]
arguments = void <|> justArgs
    where
        void :: SParser [D.Cstate]
        void = do
            token $ lift $ string "void"
            s <- get
            return $ [ D.Var s ["void"] "" Nothing ]

-- | justArgs
--
justArgs :: SParser [D.Cstate]
justArgs = (`sepBy1` comma) $ do
    (name, types) <- typeAndID
    s <- get
    return $ D.Var s types name Nothing
    where
        comma = lift $ char ','


-- | block
--
block :: SParser [D.Proc]
block = do
    sBracket
    ps <- many' $ process
    eBracket
    return ps


-- | process
--
process :: SParser D.Proc
process = funcReturn <|> callFunc <|> localVariable <|> assigne <|> aloneExp


-- | funcReturn
--
funcReturn :: SParser D.Proc
funcReturn = update $ do
    token $ lift $ string "return"
    sParen
    v <- token $ value
    eParen
    semicolon
    s <- get
    return $ D.Return s v


-- | callFunc
--
callFunc :: SParser D.Proc
callFunc = update $ do
    f <- identifire
    sParen
    a <- strLiteral
    eParen
    semicolon
    s <- get
    return $ D.Call s f [a]

-- | strLiteral
--
strLiteral :: SParser T.Text
strLiteral = token $ lift $ do
    string "\"" `liftAp` takeTill (== '"') `liftAp` "\""

-- | localVariable
--
localVariable :: SParser D.Proc
localVariable = update $ do
    v <- defVariable
    s <- get
    return $ D.LVar s v


-- | assigne
--
assigne :: SParser D.Proc
assigne = update $ do
    i <- identifire
    equal
    v <- expression
    semicolon
    s <- get
    return $ D.Assigne s i v

-- | aloneExp
--
aloneExp :: SParser D.Proc
aloneExp = update $ do
    c <- expression
    semicolon
    s <- get
    return $ D.Exprssions s c

-- | expression
--
expression :: SParser D.Exp
expression = binary <|> literal

-- | binary
--
binary :: SParser D.Exp
binary = do
    l  <- literal
    op <- operation
    r  <- literal
    return $ D.Binary l op r


-- | literal
--
literal :: SParser D.Exp
literal = D.Literal <$> value


-- | operation
--
operation :: SParser D.Operation
operation = (token $ lift $ char '+') $> D.Add


-- | sParen
--
sParen :: SParser ()
sParen = token $ lift $ char '(' $> ()

-- | eParen
--
eParen :: SParser ()
eParen = token $ lift $ char ')' $> ()

-- | sBracket
--
sBracket :: SParser ()
sBracket = token $ lift $ char '{' $> ()

-- | eBracket
--
eBracket :: SParser ()
eBracket = token $ lift $ char '}' $> ()

-- | equal
--
equal :: SParser ()
equal = token $ lift $ char '=' $> ()

-- | semicolon
--
semicolon :: SParser ()
semicolon = token $ lift $ char ';' $> ()


-- | liftAp
--
-- T.append の リフト関数
--
liftAp :: Applicative f => f T.Text -> f T.Text -> f T.Text
liftAp = liftA2 T.append



---- | preprocess
----
--preprocess :: [T.Text] -> Parser D.C
--preprocess pre = D.Prepro <$> pure pre <*> include <*> cLang pre
--
--
--
--


---- | preproIfStart
----
--preproIfStart :: Parser D.C
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
--preproIfEnd :: Parser D.C
--preproIfEnd = do
--    token $ string "#endif"
--    cLang mempty
--
--
--
--
