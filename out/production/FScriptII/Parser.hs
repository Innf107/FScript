module Parser where

import Types
import Lib
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Data.Either
import Control.Monad.Identity (Identity)

type Parser = Parsec String ()

def :: LanguageDef ()
def = emptyDef { T.commentStart = "{-"
               , T.commentEnd = "-}"
               , T.commentLine = "--"
               , T.identStart = letter <|> oneOf "_"
               , T.identLetter = alphaNum <|> oneOf "_"
               , T.opStart = oneOf "+-*/~%&§$!#<>|^°∘?:="
               , T.opLetter = oneOf "+-*/~%&§$!#<>|^°∘?:="
               , T.reservedOpNames = ["$"]
               , T.reservedNames = ["if", "then", "else", "import", "exposing", "qualified", "let", "in", "True", "False", "Null"]
               }

T.TokenParser { T.identifier = identifier
              , T.reserved = reserved
              , T.operator = operator
              , T.reservedOp = reservedOp

              , T.charLiteral = charLiteral
              , T.stringLiteral = stringLiteral
              , T.natural = natural
              , T.integer = integer
              , T.float = float
              , T.naturalOrFloat = naturalOrFloat
              , T.decimal = decimal
              , T.hexadecimal = hexadecimal
              , T.octal = octal

              , T.symbol = symbol
              , T.lexeme = lexeme
              , T.whiteSpace = whiteSpace

              , T.parens = parens
              , T.braces = braces
              , T.angles = angles
              , T.brackets = brackets
              , T.squares = squares
              , T.semi = semi
              , T.comma = comma
              , T.colon = colon
              , T.dot = dot
              , T.semiSep = semiSep
              , T.semiSep1 = semiSep1
              , T.commaSep = commaSep
              , T.commaSep1 = commaSep1
              } = T.makeTokenParser def


parseFile :: String -> String -> Either ParseError [Statement]
parseFile str fileName = parse mainParser fileName str

parseEval :: [Op] -> String -> Either ParseError Expr
parseEval ops = parse (expr ops) "EVAL"

-- TODO: Maybe add ops?
parseFileExpr :: String -> String -> Either ParseError [Statement]
parseFileExpr str fileName = fmap (map Run) (parse (many (expr [])) fileName str)

parseAsRepl :: [Op] -> String -> Either ParseError [Statement]
parseAsRepl ops = parse (statements ops) "REPL"

--TODO: Maybe add ops?
parseAsReplExpr :: String -> Either ParseError [Statement]
parseAsReplExpr str = (map Run) <$> parse (many (expr [])) "REPL" str


mainParser :: Parser [Statement]
--TODO: custom many
mainParser = whiteSpace >> (statements []) <* eof

statements :: [Op] -> Parser [Statement]
statements ops = (do
    x <- statement ops
    xs <- statements ops
    return (x:xs)
    ) <|> return []

statement :: [Op] -> Parser Statement
statement ops = (try importS <|> try (defOp ops) <|> try (defS ops) <|> try (defDestS ops) <|> try (defFClassS ops) <|> (runS ops)) <* symbol ";" <?> "statement"

importS :: Parser Statement
importS = (do
    reserved "import"
    isQ <- option False (reserved "qualified" >> return True)
    mname <- identifier
    it <- exposingI <|> return All
    return $ Import mname it isQ) <?> "import"
    where
        exposingI = do
            reserved "exposing"
            Exposing <$> commaSep (identifier <|> parens operator)

defS :: [Op] -> Parser Statement
defS ops = Def <$> (definition ops)

defOp :: [Op] -> Parser Statement
defOp ops = do
    p1 <- identifier
    op <- operator
    p2 <- identifier
    symbol "="
    e <- expr ops
    return $ Def $ NormalDef op $ Literal $ LambdaL p1 $ Literal $ LambdaL p2 e

defDestS :: [Op] -> Parser Statement
defDestS ops = do
    symbol "<<"
    name <- identifier
    v <- identifier
    symbol ">>"
    ds <- sepBy1 (expr ops) (symbol "@")
    return $ DefDest (Destr name v ds)

defFClassS :: [Op] -> Parser Statement
defFClassS ops = do
    name <- identifier <|> parens operator
    arity <- fromInteger <$> natural
    prec <- fromInteger <$> natural
    symbol "+="
    e <- expr ops
    return $ DefFClass name arity $ FClassInstance prec e

runS :: [Op] -> Parser Statement
runS ops = Run <$> (expr ops)

expr :: [Op] -> Parser Expr
expr ops = buildExpressionParser (makeOpTable ops) (term ops) <?> "expression"

data Op = Op Int Assoc

makeOpTable :: [Op] -> [[Operator String () Identity Expr]]
makeOpTable ops = [ [Infix (spaces >> return (\f x -> FCall f x)) AssocLeft]
                  , [Infix (operator >>= \p -> return (\x y -> FCall (FCall (Var p) x) y)) AssocLeft]
                  , [Infix ((symbol "`" >> identifier <* symbol "`") >>= \p -> return (\x y -> FCall (FCall (Var p) x) y)) AssocLeft]
                  , [Infix (reservedOp "$" >> return FCall) AssocRight]
                  ]

term :: [Op] -> Parser Expr
term ops =  try (Var <$> parens operator)
    <|> parens (expr ops)
    <|> Literal <$> litE ops
    <|> letE ops
    <|> ifE ops
    <|> Var <$> identifier


letE :: [Op] -> Parser Expr
letE ops = do
    reserved "let"
    d <- definition ops
    reserved "in"
    e <- (expr ops)
    return $ Let d e

definition :: [Op] -> Parser Definition
definition ops = (try normalDef <|> destDef) <?> "definition"
    where
        normalDef = do
            x <- identifier <|> parens operator
            symbol "="
            e <- (expr ops)
            return $ NormalDef x e
        destDef = do
            (dest, ps) <- parens $ do
                dest <- identifier
                ps <- many identifier
                return (dest, ps)
            symbol "="
            e <- (expr ops)
            return $ DestDef dest ps e


ifE :: [Op] -> Parser Expr
ifE ops = do
    reserved "if"
    c <- (expr ops)
    reserved "then"
    th <- (expr ops)
    reserved "else"
    el <- (expr ops)
    return $ If c th el

litE :: [Op] -> Parser Lit
litE ops = (numL <|> charL <|> (listL ops) <|> stringL <|> boolL <|> nullL  <|> lambdaL ops  <|> (recordL ops)) <?> "literal"

boolL :: Parser Lit
boolL = (reserved "True"  >> return (BoolL True))
    <|> (reserved "False" >> return (BoolL False))

numL :: Parser Lit
numL = (NumL . (fromLeftF fromInteger)) <$> naturalOrFloat

nullL :: Parser Lit
nullL = reserved "Null" >> return NullL

charL :: Parser Lit
charL = CharL <$> charLiteral

listL :: [Op] -> Parser Lit
listL ops = ListL <$> brackets (commaSep (expr ops))

stringL :: Parser Lit
stringL = ListL <$> (fmap (Literal . CharL) <$> stringLiteral)

recordL :: [Op] -> Parser Lit
recordL ops = fmap RecordL $ braces $ commaSep $ do
    k <- identifier <|> between (char '\"') (char '\"') (many alphaNum)
    symbol ":"
    v <- (expr ops)
    return (k, v)

lambdaL :: [Op] -> Parser Lit
lambdaL ops = do
    symbol "\\"
    x <- identifier
    symbol "->"
    e <- expr ops
    return $ LambdaL x e
