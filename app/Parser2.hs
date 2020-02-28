module Parser2 where

import Types
import Lib
import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T
import Text.Parsec.Language
import Data.Either

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

parseEval :: String -> Either ParseError Expr
parseEval str = parse expr "EVAL" str

parseFileExpr :: String -> String -> Either ParseError [Statement]
parseFileExpr str fileName = fmap (map Run) (parse (many (expr)) fileName str)

parseAsRepl :: String -> Either ParseError [Statement]
parseAsRepl = parse mainParser "REPL"

--TODO: many expr seems weird
parseAsReplExpr :: String -> Either ParseError [Statement]
parseAsReplExpr str = (map Run) <$> parse (many expr) "REPL" str

mainParser :: Parser [Statement]
mainParser = whiteSpace >> (many statement) <* eof

statement :: Parser Statement
statement = (try importS <|> try defS <|> try defDestS <|> try defFClassS <|> runS) <* symbol ";" <?> "statement"

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

defS :: Parser Statement
defS = Def <$> definition

defDestS :: Parser Statement
defDestS = do
    symbol "<<"
    name <- identifier
    v <- identifier
    symbol ">>"
    ds <- sepBy1 expr (symbol "@")
    return $ DefDest (Destr name v ds)

defFClassS :: Parser Statement
defFClassS = do
    name <- identifier <|> parens operator
    arity <- fromInteger <$> natural
    prec <- fromInteger <$> natural
    symbol "+="
    e <- expr
    return $ DefFClass name arity $ FClassInstance prec e

runS :: Parser Statement
runS = Run <$> expr

expr :: Parser Expr
expr = buildExpressionParser exTable term <?> "expression"

exTable = [ [Infix (spaces >> return (\f x -> FCall f x)) AssocLeft]
          , [Infix (operator >>= \p -> return (\x y -> FCall (FCall (Var p) x) y)) AssocLeft]
          , [Infix ((symbol "`" >> identifier <* symbol "`") >>= \p -> return (\x y -> FCall (FCall (Var p) x) y)) AssocLeft]
          , [Infix (reservedOp "$" >> return FCall) AssocRight]
          ]

term :: Parser Expr
term =  try (Var <$> parens operator)
    <|> parens expr
    <|> Literal <$> litE
    <|> letE
    <|> ifE
    <|> Var <$> identifier


letE :: Parser Expr
letE = do
    reserved "let"
    d <- definition
    reserved "in"
    e <- expr
    return $ Let d e

definition :: Parser Definition
definition = (try normalDef <|> destDef) <?> "definition"
    where
        normalDef = do
            x <- identifier <|> parens operator
            symbol "="
            e <- expr
            return $ NormalDef x e
        destDef = do
            (dest, ps) <- parens $ do
                dest <- identifier
                ps <- many identifier
                return (dest, ps)
            symbol "="
            e <- expr
            return $ DestDef dest ps e


ifE :: Parser Expr
ifE = do
    reserved "if"
    c <- expr
    reserved "then"
    th <- expr
    reserved "else"
    el <- expr
    return $ If c th el

litE :: Parser Lit
litE = (numL <|> charL <|> listL <|> stringL <|> boolL <|> nullL  <|> lambdaL  <|> recordL) <?> "literal"

boolL :: Parser Lit
boolL = (reserved "True"  >> return (BoolL True))
    <|> (reserved "False" >> return (BoolL False))

numL :: Parser Lit
numL = (NumL . (fromLeftF fromInteger)) <$> naturalOrFloat

nullL :: Parser Lit
nullL = reserved "Null" >> return NullL

charL :: Parser Lit
charL = CharL <$> charLiteral

listL :: Parser Lit
listL = ListL <$> brackets (commaSep expr)

stringL :: Parser Lit
stringL = ListL <$> (fmap (Literal . CharL) <$> stringLiteral)

recordL :: Parser Lit
recordL = fmap RecordL $ braces $ commaSep $ do 
    k <- identifier <|> between (char '\"') (char '\"') (many alphaNum)
    symbol ":"
    v <- expr
    return (k, v)

lambdaL :: Parser Lit
lambdaL = do
    symbol "\\"
    x <- identifier
    symbol "->"
    e <- expr
    return $ LambdaL x e

