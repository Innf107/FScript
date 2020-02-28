{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
module Parser where

import Text.Parsec (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.String.Parsec
import Text.Parsec.String.Char
import Text.Parsec.String.Combinator
import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Char (isLetter, isDigit)
import Data.Maybe
import Data.Either
--import Debug.Trace
import Lib
import Text.Parsec.Prim ((<?>))
import Control.DeepSeq
import qualified Data.Map as M
import Types


parseFile :: String -> String -> Either ParseError [Statement]
parseFile str fileName = (parse (many (statement)) fileName str)

parseFileExpr :: String -> String -> Either ParseError [Statement]
parseFileExpr str fileName = fmap (map Run) (parse (many (expr)) fileName str)

parseEval :: String -> Either ParseError Expr
parseEval str = parse expr "EVAL" str

parseAsRepl :: String -> Either ParseError [Statement]
parseAsRepl = parse (many (statement)) "REPL"

parseAsReplExpr :: String -> Either ParseError [Statement]
parseAsReplExpr = fmap (fmap Run) . parse (many (expr)) "REPL"

-- Identifier
reservedIDs :: [String]
reservedIDs = ["if", "then", "else", "import", "exposing", "qualified", "let", "in", ";", ",", "->"]

idChar :: Parser Char 
idChar = alphaNum <|> oneOf "._'¹²³⁴⁵⁶⁷⁸⁹⁰"

identifier :: Parser String
identifier = noPof (do keyword <$> reservedIDs) (do {x <- letter; xs <- many idChar; return (x:xs)}) <?> "identifier"

keyword :: String -> Parser String
keyword s = (do
    x <- string s
    notFollowedBy idChar
    return x) <?> "keyword" 
    
moduleIdentifier :: Parser String
moduleIdentifier = many (idChar <|> oneOf "/~.") <?> "module identifier"

operator :: Parser String
operator = (many1 $ oneOf "+-_*/~%&$§!#<>|^°∘?:=") <?> "operator"

-- Statement
statement :: Parser Statement
statement = do
    spaces 
    s <- try commentS <|> statement'
    spaces
    return s -- <|> return NOP

statement' :: Parser Statement
statement' = do
    spaces
    s <- try importS <|> try defS <|> try defDestS <|> try defFClassS <|> try runS
    spaces
    char ';'
    return s

defDestS :: Parser Statement
defDestS = do
            string "<<"
            spaces
            name <- identifier
            spaces
            v <- identifier
            spaces
            string ">>"
            spaces
            ds <- sepBy expr (char '@')
            return $ DefDest $ Destr name v ds


defFClassS :: Parser Statement
defFClassS = do
            name <- identifier <|> operator
            spaces
            arity <- read <$> (many1 digit)
            spaces
            prec <- read <$> (many1 digit)
            spaces
            string "+="
            spaces
            e <- expr
            return $ DefFClass name arity $ FClassInstance prec e

commentS :: Parser Statement
commentS = (spaces >> string "--" >> many (noneOf "\n") >> return NOP) <?> "comment"

importS :: Parser Statement
importS = do
    string "import"
    spaces
    isQ <- option False (const True <$> string "qualified")
    spaces
    mname <- moduleIdentifier
    spaces
    it <- importTypeS
    return $ Import mname it isQ

importTypeS :: Parser ImportType
importTypeS = try exposingI <|> return All

exposingI :: Parser ImportType
exposingI = do
    string "exposing"
    spaces
    ids <- sepBy (identifier <|> operator) (char ',' >> spaces)
    return $ Exposing ids

defS :: Parser Statement
defS = do
    spaces
    normalDef <|> destDef
    where
        normalDef = do
            name <- identifier <|> operator
            spaces
            char '='
            spaces
            e <- expr
            return $ Def $ NormalDef name e
        destDef = do
            char '('
            dest <- identifier
            spaces
            pars <- sepBy1 identifier spaces
            char ')'
            spaces
            char '='
            spaces
            e <- expr
            return $ Def $ DestDef dest pars e

runS :: Parser Statement
runS = do
    v <- expr
    return $ Run v


-- Expression
expr :: Parser Expr
expr = do
    spaces
    ex <- expr'
    args <- many (try expr')
    return $ genFCall ex (reverse args)
    where
        genFCall :: Expr -> [Expr] -> Expr
        genFCall ex [] = ex
        genFCall ex (a:as) = FCall (genFCall ex as) a

expr' :: Parser Expr
expr' = do
    spaces
    ex <- try (expr'' `chainl1` infixE) <|> expr''
    spaces
    return ex

expr'' :: Parser Expr
expr'' = try (parensE <|> litE <|> letE <|> ifE) <|> varE -- <|> fcallE {- try (chainl1 (try expr') (try infixE)) <|> -}

-- {f x y z}
-- FCall {f x y} (Var z)
-- FCall (FCall {f x} (Var y)) (Var z)
-- FCall (FCall (FCall {f} (Var x)) (Var y)) (Var z)
-- FCall (FCall (FCall (Var f) (Var x)) (Var y)) (Var z)

manyBounded :: Parser a -> Int -> Parser [a]
manyBounded _ 0 = return []
manyBounded p n = do
    r <- p
    rest <- manyBounded p (n - 1)
    return (r:rest)


parensE :: Parser Expr
parensE = between (char '(') (char ')') expr

letE :: Parser Expr
letE = do
    spaces
    string "let"
    spaces
    normalLet <|> destLet
    where
        normalLet = do
            name <- identifier
            spaces
            char '='
            spaces
            vex <- expr
            spaces
            try $ string "in"
            spaces
            e <- expr
            spaces
            return $ Let (NormalDef name vex) e
        destLet = do
            char '('
            dest <- identifier
            spaces
            pars <- sepBy1 identifier spaces
            char ')'
            spaces
            char '='
            spaces
            vex <- expr
            spaces
            try $ string "in"
            spaces
            e <- expr
            spaces
            return $ Let (DestDef dest pars vex) e


ifE :: Parser Expr
ifE = do
    spaces
    string "if"
    spaces
    c <- expr
    spaces
    string "then"
    spaces
    -- TODO: BUG: Needs Parens for infix?!?
    th <- expr
    spaces
    string "else"
    spaces
    el <- expr
    spaces
    return $ If c th el

varE :: Parser Expr
varE = do
    name <- identifier <|> operator
    return $ Var name


infixE :: Parser (Expr -> Expr -> Expr)
infixE = do
    spaces
    op <- (operator <|> backtick)
    spaces
    return $ (\e1 e2 -> FCall (FCall (Var op) e1) e2)
    where
        backtick = do
            char '`'
            op <- identifier
            char '`'
            return op

-- Obsolete
fcallE :: Parser Expr
fcallE = do
    f <- expr'
    spaces
    a <- expr
    return $ FCall f a

-- f x y z
-- ((f x) y) z

-- x + y + z
-- (x + y) + z
-- + (+ x y) z
-- (+ ((+ x) y) z

(<++>) a b = (++) <$> a <*> b
(<:>)  a b = (:)  <$> a <*> b

-- Literals
litE :: Parser Expr
litE = Literal <$> (try spaces >> (numL <|> boolL <|> nullL <|> charL <|> listL <|> stringL <|> lambdaL <|> recordL))

numL :: Parser Lit
numL = (NumL . read) <$> (int <++> decimal <++> exponent)
    where decimal = option "" $ char '.' <:> number
          exponent = option "" $ oneOf "Ee" <:> number
          int    = (plus <|> minus <|> number)
          plus   = char '+' >> number
          minus  = char '-' <:> number
          number = many1 digit


boolL :: Parser Lit
boolL = do
        x <- string "True" <|> string "False"
        spaces
        return $ case x of
            "True" -> BoolL True
            "False"-> BoolL False

nullL :: Parser Lit
nullL = do
        string "Null"
        spaces
        return NullL


charL :: Parser Lit
charL = do
         char '\''
         x <- escapeC <|> noneOf "'"
         char '\''
         return $ CharL x
         
escapeC :: Parser Char
escapeC = do
    char '\\'
    x <- satisfy (const True)
    case lookup x escapes of
        Nothing -> fail $ "Invalid escape code \\" ++ [x]
        Just e -> return e
         
escapes :: [(Char, Char)]
escapes = [('n', '\n'), ('t', '\t'), ('\\', '\\'), ('\'', '\''), ('"', '"'), ('p', 'π')]
         
listL :: Parser Lit
listL = do
        char '['
        exps <- catMaybes <$> sepBy ((Just <$> expr) <|> return Nothing) (char ',' >> spaces)
        char ']'
        return $ ListL exps

stringL :: Parser Lit
stringL = do
            char '"'
            chars <- many $ (escapeC <|> noneOf ['"'])
            char '"'
            return $ ListL (Literal . CharL <$> chars)


lambdaL :: Parser Lit
lambdaL = do
    char '\\'
    spaces
    param <- identifier
    spaces
    string "->"
    spaces
    e <- expr
    return $ LambdaL param e

recordL :: Parser Lit
recordL = do
    char '{'
    spaces
    elems <- (flip sepBy) (char ',')  (do 
            spaces
            name <- identifier <|> do
                char '"'
                x <- identifier
                char '"'
                return x
            spaces
            char ':'
            spaces
            val <- expr
            spaces
            return (name, val))
    spaces
    char '}'
    return $ RecordL elems
