{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Language.C.Analysis.Light
( analyze
, program
, token
, defVariable
, defFunction
, identifire
, preprocess
, expr
, ifStatement
) where

import           Control.Applicative
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.State      (StateT, evalStateT, get,
                                                 modify)
import           Data.Attoparsec.Expr
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
analyze :: T.Text -> Either String D.Ast
analyze s = case parse (p <* endOfInput) s `feed` "" of
    (Done _ r)    -> Right r
    (Fail i ss w) -> Left $ intercalate " : " ((show i):w:ss)
    (Partial _)   -> Left "partial ..."
    where
        p = (`evalStateT` []) program

-- | program
--
program :: SParser D.Ast
program = D.Ast <$> many' (preprocess <|> statement)


-- | statement
--
statement :: SParser D.Statement
statement = defVariable <|>  defFunction

-- | preprocess
--
preprocess :: SParser D.Statement
preprocess = D.Preprocess <$> (include <|> define)


-- | include
--
include :: SParser D.PreState
include =  D.Include <$> get <* incKey <*> file <* tillEndOfLine
    where
        incKey = token $ lift $ string "#include"

-- | file
--
file :: SParser T.Text
file = envPath <|> strText
    where envPath = lift (string "<") `liftAp` identifire `liftAp` lift (string ".h") `liftAp` lift (string ">")


-- | define
--
define :: SParser D.PreState
define = D.Define <$> get <* p <*> identifire <*> text
    where
        p = lift $ skipSpace *> string "#define"

--  text
--
text :: SParser T.Text
text = end <|> T.cons <$> fetch <*> text
    where
        end = lift (char '\n' *> pure "")
        fetch = lift anyChar



-- | tillEndOfLine
--
tillEndOfLine :: SParser ()
tillEndOfLine = lift $ takeTill isEndOfLine *> endOfLine



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
    e <- expr
    return $ D.Condition c e

-- | condEnd
--
condEnd :: SParser ()
condEnd = token $ lift $ string "#endif" *> pure ()





-- | defVariable
--
defVariable :: SParser D.Statement
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
    t <- identifire
    ts <- many1 $ arrayAndId <|> singleton identifire <|> singleton pointer
    let ids = t : (concat ts)
    return (last ids, init ids)
    where
        singleton p = fmap (:[]) p


-- | arrayAndId
--
arrayAndId :: SParser [T.Text]
arrayAndId = do
    ident <- identifire
    arr <- array
    return $ [arr, ident]

-- | array
--
array :: SParser T.Text
array = do
    lift $ char '['
    elem <- identifire <|> numberWord
    lift $ char ']'
    return $ "[" `T.append` elem `T.append` "]"




-- | pointer
--
pointer :: SParser T.Text
pointer = token $ lift $ string "*"



-- | initValue
--
initValue :: SParser (Maybe D.Exp)
initValue = Just <$> p <|> pure Nothing
    where
        p :: SParser D.Exp
        p = equal *> expr


-- | defFunction
--
defFunction :: SParser D.Statement
defFunction = do
    (name, ret) <- typeAndID
    args <- parens arguments
    p <- block
    s <- get
    return $ D.Func s ret name args p


-- | arguments
--
arguments :: SParser [D.Statement]
arguments = void <|> justArgs
    where
        void :: SParser [D.Statement]
        void = do
            token $ lift $ string "void"
            s <- get
            return $ [ D.Var s ["void"] "" Nothing ]

-- | justArgs
--
justArgs :: SParser [D.Statement]
justArgs = (`sepBy1` comma) $ do
    (name, types) <- typeAndID
    s <- get
    return $ D.Var s types name Nothing
    where


-- | block
--
block :: SParser [D.Proc]
block = brackets $ many' process


-- | process
--
process :: SParser D.Proc
process = funcRet <|> defLVariable <|> exprState
    where
        defLVariable = D.LVar <$> get <*> defVariable
        funcRet = D.Return <$> get <* retKey <*> expr <* semicolon


-- | exprState
--
exprState :: SParser D.Proc
exprState = update $ D.ExpState <$> get <*> expr <* semicolon

-- | ifStatement
--
ifStatement :: SParser D.Proc
ifStatement = update $ D.IfStatement <$ ifKey <*> get <*> parens expr <*> block
    where
        ifKey = token $ lift $ string "if"


-- ----------------------------------------------------------------------------------
-- | expression
-- ----------------------------------------------------------------------------------

-- | expr
--
expr :: SParser D.Exp
expr = lift expr'

expr' :: Parser D.Exp
expr' = buildExpressionParser table term <?> "expression"

-- TODO:関数コールもテーブルに含めたい
--
term :: Parser D.Exp
term =  parens' expr' <|> call' <|> id' <|> literal' <|> strLiteral' <?> "simple expression"
    where
        id' = D.Identifire <$> identifire'

table :: [[Operator T.Text D.Exp]]
table = [ [postfix "++" (D.PostUnary "++")]                                                         -- 1  : Left
        , [prefix  "&"  (D.PreUnary "&")]                                                         -- 2
        , [binary' "*"  (D.Binary "*" ) AssocLeft, binary' "/" (D.Binary "/") AssocLeft]          -- 3  : Left
        , [binary' "+"  (D.Binary "+" ) AssocLeft, binary' "-" (D.Binary "-") AssocLeft]          -- 4  : Left
        , [binary' "==" (D.Binary "==") AssocRight]                                               -- 7  : Left
        , [binary' "="  (D.Binary "=" ) AssocRight]                                               -- 14 : Right
        ]
--table = [ [prefix "-" negate, prefix "+" id ]
--        , [postfix "++" (+1)]
--        , [binary' "*" (*) AssocLeft, binary' "/" (div) AssocLeft ]
--        , [binary' "+" (+) AssocLeft, binary' "-" (-)   AssocLeft ]
--        ]

binary' :: T.Text -> (D.Exp -> D.Exp -> D.Exp) -> Assoc -> Operator T.Text D.Exp
binary' name fun assoc = Infix (do{ string name; return fun }) assoc
prefix  name fun       = Prefix (do{ string name; return fun })
postfix name fun       = Postfix (do{ string name; return fun })

-- | call'
call' :: Parser D.Exp
call' = do
    f <- identifire'
    as <- parens' $ expr' `sepBy` comma'
    return $ D.Call f as



-- | literal
--
literal :: SParser D.Exp
literal = lift literal'

literal' :: Parser D.Exp
literal' = token' $ D.Literal <$> numberWord'

-- | strLiteral
--
strLiteral :: SParser D.Exp
strLiteral = lift strLiteral'

strLiteral' :: Parser D.Exp
strLiteral' = token' $ D.StrLiteral <$> strText'

-- | strText
--
strText :: SParser T.Text
strText = lift strText'

strText' :: Parser T.Text
strText' = char '"' *> takeTill (== '"') <* char '"'


-- | identifire
--
-- TODO:1文字識別子だとバグる
--
identifire :: SParser T.Text
identifire = lift identifire'

identifire' :: Parser T.Text
identifire' = token' $ do
    head' <- letter <|> char '_'
    tail' <- many1 idLetter'
    return $ T.pack $ head' : tail'

-- | idLetter'
--
idLetter' :: Parser Char
idLetter' = letter <|> digit <|> char '_'


-- | numberWord
--
numberWord :: SParser T.Text
numberWord = lift numberWord'

numberWord' :: Parser T.Text
numberWord' = hex' <|> integer'

-- | integer
--
integer :: SParser T.Text
integer = lift integer'

integer' :: Parser T.Text
integer' = token' $ T.pack <$> many1 digit

-- | hex
--
hex :: SParser T.Text
hex = lift hex'

hex' :: Parser T.Text
hex' = T.append <$> string "0x" <*> p
    where
        p = fmap T.pack $ many1 $ satisfy $ inClass "a-fA-F0-9"

-- ----------------------------------------------------------------------------------
-- | keywords
-- ----------------------------------------------------------------------------------
-- | parens
--
parens :: SParser a -> SParser a
parens p = sParen *> p <* eParen
    where
        sParen = token $ lift $ char '(' $> ()
        eParen = token $ lift $ char ')' $> ()

parens' :: Parser a -> Parser a
parens' p = sParen' *> p <* eParen'
    where
        sParen' = token' $ char '(' $> ()
        eParen' = token' $ char ')' $> ()


-- | retKey
--
retKey :: SParser ()
retKey = token $ lift $ string "return" $> ()


-- | comma
--
comma :: SParser Char
comma = lift comma'

-- | comma'
--
comma' :: Parser Char
comma' = char ','

-- | brackets
--
brackets :: SParser a -> SParser a
brackets p = sBracket *> p <* eBracket
    where
        sBracket = token $ lift $ char '{' $> ()
        eBracket = token $ lift $ char '}' $> ()


brackets' :: Parser a -> Parser a
brackets' p = sBracket' *> p <* eBracket'
    where
        sBracket' = token' $ char '{' $> ()
        eBracket' = token' $ char '}' $> ()

-- | equal
--
equal :: SParser ()
equal = lift equal'

-- | equal'
--
equal' :: Parser ()
equal' = token' $ char '=' $> ()

-- | semicolon
--
semicolon :: SParser ()
semicolon = token $ lift $ char ';' $> ()



-- ----------------------------------------------------------------------------------
-- | lexer
-- ----------------------------------------------------------------------------------

-- | token
--
-- State + Parser 用のトークン関数
--
token :: SParser a -> SParser a
token p = lift spaceOrComment *> p <* lift spaceOrComment

-- | token'
--
-- Parser 単体用のトークン関数
--
token' :: Parser a -> Parser a
token' p = spaceOrComment *> p <* spaceOrComment

-- | spaceOrComment
--
spaceOrComment :: Parser ()
spaceOrComment = skipMany $
    space $> () <|> comment1 <|> comment2

-- | comment1
--
comment1 :: Parser ()
comment1 = do
    string "/*"
    consume
    where
        consume = string "*/" $> () <|> anyChar *> consume

-- | comment2
--
-- // ~~~~~~~~~~~~~
--
comment2 :: Parser ()
comment2 = string "//" *> takeTill isEndOfLine *> endOfLine


-- ----------------------------------------------------------------------------------
-- | 補助関数
-- ----------------------------------------------------------------------------------

-- | liftAp
--
-- T.append の リフト関数
--
liftAp :: Applicative f => f T.Text -> f T.Text -> f T.Text
liftAp = liftA2 T.append





