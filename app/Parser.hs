{-# LANGUAGE FlexibleInstances #-}
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

data Statement = NOP
               | Import String ImportType Bool
               | Def String Expr
               | Run Expr
               deriving (Show, Eq, Read)

data ImportType = All
                | Exposing [String]
    deriving (Show, Eq, Read)

data Expr = Literal Lit
          | Let String Expr Expr
          | If Expr Expr Expr
          | Var String
          | FCall Expr Expr
          | Value RTValue
          deriving (Show, Eq, Read)

data Lit = IntL Integer
         | CharL Char
         | ListL [Expr]
         | BoolL Bool
         | NullL
         | LambdaL String Expr
         | MapL [(String, Expr)]
         deriving (Show, Eq, Read)

data RTValue = IntV Integer
             | CharV Char
             | ListV [RTValue]
             | BoolV Bool
             | IOV IOAction
             | FuncV String Expr [(String, RTValue)]
             | NativeF (RTValue -> RTState -> RTValue) [(String, RTValue)]
             | NullV
             | MapV [(String, RTValue)]
             | Exception String String
             deriving (Show, Eq, Read)

data RTState = RTState {getVals::[(String, RTValue)], getArgs::[(String, RTValue)], getClosures::[(String, RTValue)]} deriving (Show, Eq)

data IOAction = Print String
              | ReadLine 
              | Composed IOAction RTValue
              deriving (Show, Eq, Read)

instance Show (RTValue -> RTState -> RTValue) where
    show _ = "(RTValue -> RTState -> RTValue)"
instance Eq (RTValue -> RTState -> RTValue) where
    _ == _ = False

instance Read (RTValue -> RTState -> RTValue) where
    readsPrec _ _ = []

parseFile :: String -> String -> Either ParseError [Statement]
parseFile str fileName = (parse (many (statement)) fileName str)

parseFileExpr :: String -> String -> Either ParseError [Statement]
parseFileExpr str fileName = fmap (map Run) (parse (many (expr)) fileName str)


parseAsRepl :: String -> Either ParseError [Statement]
parseAsRepl = parse (many (statement)) "REPL"

parseAsReplExpr :: String -> Either ParseError [Statement]
parseAsReplExpr = fmap (map Run) . parse (many (expr)) "REPL"

-- Identifier
reservedIDs :: [String]
reservedIDs = ["if", "then", "else", "import", "exposing", "qualified", "let", "in"]

identifier :: Parser String
identifier = noPof (string <$> reservedIDs) (do {x <- letter; xs <- many (alphaNum <|> oneOf "._"); return (x:xs)})


-- Statement
statement :: Parser Statement
statement = do 
    spaces 
    s <- try commentS <|> statement'
    spaces
    return s-- <|> return NOP

statement' :: Parser Statement
statement' = do
    spaces
    s <- try importS <|> try defS <|> try runS
    spaces
    char ';'
    return s


commentS :: Parser Statement
commentS = spaces >> string "--" >> many (noneOf "\n") >> return NOP

importS :: Parser Statement
importS = do
    string "import"
    spaces
    isQ <- option False (const True <$> string "qualified")
    spaces
    mname <- identifier
    spaces
    it <- importTypeS
    return $ Import mname it isQ

importTypeS :: Parser ImportType
importTypeS = try exposingI <|> return All

exposingI :: Parser ImportType
exposingI = do
    string "exposing"
    spaces
    ids <- sepBy identifier (char ',' >> spaces)
    return $ Exposing ids

defS :: Parser Statement
defS = do
    name <- identifier
    spaces
    char '='
    spaces
    v <- expr
    return $ Def name v

runS :: Parser Statement
runS = do
    v <- expr
    return $ Run v


-- Expression
expr :: Parser Expr
expr = do
    ex <- expr'
    args <- catMaybes <$> manyBounded (parseMaybe expr') 100
    return $ genFCall ex (reverse args)
    where
        genFCall :: Expr -> [Expr] -> Expr
        genFCall ex [] = ex
        genFCall ex (a:as) = FCall (genFCall ex as) a

expr' :: Parser Expr
expr' = do
    spaces
    ex <- try (parensE <|> litE <|> letE <|> ifE) <|> try varE -- <|> fcallE {- try (chainl1 (try expr') (try infixE)) <|> -}
    spaces
    return ex

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
    name <- identifier
    spaces
    char '='
    spaces
    vex <- expr
    spaces
    string "in"
    spaces
    e <- expr
    return $ Let name vex e

ifE :: Parser Expr
ifE = do
    spaces
    string "if"
    spaces
    c <- expr
    spaces
    string "then"
    spaces
    th <- expr
    spaces
    string "else"
    spaces
    el <- expr
    spaces
    return $ If c th el

varE :: Parser Expr
varE = do
    name <- identifier
    return $ Var name


infixE :: Parser (Expr -> Expr -> Expr)
infixE = do
    spaces
    op <- (operatorE <|> backtickE)
    spaces
    return $ (\e1 e2 -> FCall (FCall (Var op) e1) e2)
    where
        operatorE = many1 (oneOf "+-/|<>^°~*.")
        backtickE = do
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

-- Literals
litE :: Parser Expr
litE = Literal <$> (try spaces >> (intL <|> boolL <|> nullL <|> charL <|> listL <|> stringL <|> lambdaL <|> mapL))

intL :: Parser Lit
intL = do
        sign <- option "" (string "-")
        x <- many1 digit
        return $ IntL (read (sign ++ x))

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
         x <- satisfy (const True)
         char '\''
         return $ CharL x
         
listL :: Parser Lit
listL = do
        char '['
        exps <- catMaybes <$> sepBy ((Just <$> expr) <|> return Nothing) (char ',' >> spaces)
        char ']'
        return $ ListL exps

stringL :: Parser Lit
stringL = do
            char '"'
            chars <- many $ noneOf ['"']
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

mapL :: Parser Lit
mapL = do
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
    return $ MapL elems
