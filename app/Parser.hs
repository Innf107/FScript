{-# LANGUAGE BlockArguments #-}
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
import Data.List as L
import Debug.Trace (traceShow)
import Data.Bifunctor

type Parser = Parsec String ()

def :: LanguageDef ()
def = emptyDef { T.commentStart = "{-"
               , T.commentEnd = "-}"
               , T.commentLine = "--"
               , T.identStart = letter <|> oneOf "_"
               , T.identLetter = alphaNum <|> oneOf "_"
               , T.opStart = oneOf "+-*/~%&§$!#<>|^°∘?:="
               , T.opLetter = oneOf "+-*/~%&§$!#<>|^°∘?:="
               , T.reservedOpNames = ["$", "<-", "~", "&", "->", "|", "@"]
               , T.reservedNames = ["do", "if", "then", "else", "import", "exposing", "qualified", "let", "in", "infixl", "infixr", "True", "False", "Null"]
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

parseAsRepl :: [Op] -> String -> Either ParseError ([Statement], [Op])
parseAsRepl ops = parse (statements ops) "REPL"

--TODO: Maybe add ops?
parseAsReplExpr :: String -> Either ParseError [Statement]
parseAsReplExpr str = (map Run) <$> parse (many (expr [])) "REPL" str


mainParser :: Parser [Statement]
mainParser = whiteSpace >> (fst <$> statements []) <* eof

statements :: [Op] -> Parser ([Statement], [Op])
statements ops = whiteSpace >> ((do
    x <- statement ops
    case x of
        [DefOpPrec op prec assoc] -> (statements ((Op op prec assoc):ops))
        _ -> (first (x++) <$> statements ops)
    ) <|> return ([], ops))

statement :: [Op] -> Parser [Statement]
statement ops =
    ((try (pure <$> try defOpPrec)
    <|> try (pure <$> importS)
    <|> try (pure <$> (defOp ops))
    <|> try (pure <$> (defS ops))
    <|> try (pure <$> (defDestS ops))
    <|> try (defADT ops)
    <|> try (pure <$> (defFClassS ops))
    <|> (pure <$> ((runS ops)))) <* symbol ";") <?> "statement"

defADT :: [Op] -> Parser [Statement]
defADT ops = do
    tname <- identifier
    symbol ":="
    (adtConst tname) `sepBy1` (symbol "|") 
    where
        adtConst :: String -> Parser Statement
        adtConst tname = do
            --TODO: maybe first uppercase?
            cname <- identifier
            ps <- many adtParam
            return $ constructADT tname cname ps
        adtParam = do
            n <- identifier
            symbol ":"
            t <- identifier
            return (n, t)
        constructADT :: String -> String -> [(String, String)] -> Statement
        constructADT tname cname ps = Def $ NormalDef cname $ FCall (FCall (Var "tclass") (Literal $ ListL $ (strAsEx . snd) <$> ps)) $
           constructADT' tname cname (fst <$> ps) (fst <$> ps)
        constructADT' :: String -> String -> [String] -> [String] -> Expr
        constructADT' tname cname [] ips = Literal $ RecordL $ [("Type", strAsEx tname), ("Variant", strAsEx cname)] ++ ((\n -> (n, Var n)) <$> ips)
        constructADT' tname cname (p:ps) ips = Literal $ LambdaL p $ constructADT' tname cname ps ips

defOpPrec :: Parser Statement
defOpPrec = do
            assoc <- (reserved "infixl" >> return AssocLeft) <|> (reserved "infixr" >> return AssocRight)
            op <- operator
            prec <- natural
            return $ DefOpPrec op prec assoc


importS :: Parser Statement
importS = (do
    reserved "import"
    isQ <- option False (reserved "qualified" >> return True)
    mname <- many $ (alphaNum <|> oneOf "/.")
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
defFClassS ops = try defFClassE <|> defFClassI
    where
        defFClassE = do
            name <- identifier <|> parens operator
            arity <- ((+ (-1)) . fromInteger) <$> natural
            prec <- fromInteger <$> natural
            symbol "+="
            e <- expr ops
            return $ DefFClass name arity $ FClassInstance prec e
        defFClassI = do
            name <- identifier <|> parens operator
            prec <- fromInteger <$> natural
            ps <- many (identifier <|> parens operator)
            symbol "+="
            e <- (expr ops)
            return $ DefFClass name (length ps - 1) $ genF ps e prec
        genF :: [String] -> Expr -> Int -> FClassInstance
        genF []     e pr = FClassInstance pr e
        genF (p:ps) e pr = genF ps (Literal $ LambdaL p e) pr



runS :: [Op] -> Parser Statement
runS ops = Run <$> (expr ops)

expr :: [Op] -> Parser Expr
expr ops = buildExpressionParser (makeOpTable ops) (term ops) <?> "expression"

makeOpTable :: [Op] -> [[Operator String () Identity Expr]]
makeOpTable ops = [ [Prefix (reservedOp "~" >> return (FCall $ Var "flip"))]
                  , [Infix (spaces >> return (\f x -> FCall f x)) AssocLeft]
                  , [Prefix (symbol "-" >> return (\x -> FCall (FCall (Var "-") (Literal (NumL 0))) x))]
                  ]
                  ++
                  opsToTable ops
                  ++
                  [ [Infix (operator >>= \p -> return (\x y -> FCall (FCall (Var p) x) y)) AssocLeft]
                  , [Infix ((symbol "`" >> identifier <* symbol "`") >>= \p -> return (\x y -> FCall (FCall (Var p) x) y)) AssocLeft]
                  , [Infix (reservedOp "$" >> return FCall) AssocRight]
                  , [Infix (reservedOp "&" >> return (\x f -> FCall f x)) AssocLeft]
                  ]

opsToTable :: [Op] -> [[Operator String () Identity Expr]]
opsToTable ops = (`map` opTable) (map (\(Op op _ assoc) -> Infix (symbol op >>= \p -> return (\x y -> FCall (FCall (Var p) x) y)) assoc))
    where opTable = L.groupBy (\(Op _ p1 _) (Op _ p2 _) -> p1 == p2) ops

term :: [Op] -> Parser Expr
term ops =  try (Var <$> parens operator)
    <|> parens (expr ops)
    <|> try (interpStringE ops)
    <|> Literal <$> litE ops
    <|> letE ops
    <|> ifE ops
    <|> doE ops
    <|> Var <$> identifier

data DoVar = DoVar String Expr | DoEx Expr

doE :: [Op] -> Parser Expr
doE ops = try (bracketDoE ops) <|> pipeDoE ops
    where
    bracketDoE :: [Op] -> Parser Expr
    bracketDoE ops = do
        reserved "do"
        doVs <- braces $ semiSep (try doVar <|> fmap DoEx (expr ops))
        return (createDo doVs)

    pipeDoE :: [Op] -> Parser Expr
    pipeDoE ops = do
        reserved "do"
        optional (reservedOp "|")
        doVs <- (try doVar <|> fmap DoEx (expr ops)) `sepBy1` (reservedOp "|")
        return $ createDo doVs
        
    doVar = do
        x <- identifier
        reservedOp "<-"
        e <- expr ops
        return $ DoVar x e
    createDo :: [DoVar] -> Expr
    createDo ((DoEx e):[]) = e
    createDo ((DoVar _ e):[]) = e
    createDo ((DoEx e):dvs) = (FCall (FCall (Var ">>=") e) (Literal (LambdaL "_" (createDo dvs))))
    createDo ((DoVar i e):dvs) = (FCall (FCall (Var ">>=") e) (Literal (LambdaL i (createDo dvs))))

letE :: [Op] -> Parser Expr
letE ops = (do
    reserved "let"
    d <- definition ops
    reserved "in"
    e <- (expr ops)
    return $ Let d e) <?> "let expression"

definition :: [Op] -> Parser Definition
definition ops = (try normalDef <|> destDef) <?> "definition"
    where
        normalDef = do
            x <- identifier <|> parens operator
            ps <- many (identifier <|> parens operator)
            symbol "="
            e <- (expr ops)
            return $ NormalDef x (genF ps e)
        destDef = do
            (dest, ps) <- parens $ do
                dest <- identifier
                ps <- many identifier
                return (dest, ps)
            symbol "="
            e <- (expr ops)
            return $ DestDef dest ps e
        genF :: [String] -> Expr -> Expr
        genF []     e = e
        genF (p:ps) e = Literal $ LambdaL p $ genF ps e


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

interpStr :: [Op] -> Parser [Expr]
interpStr ops = char '@' >> between (char '\"') (char '\"') interpStrInner
    where
        interpStrInner :: Parser [Expr]
        interpStrInner = many $ (asEx <$> many1 (noneOf "$\"")) <|> (char '$' >> between (char '{') (char '}') (expr ops))
        asEx :: String -> Expr
        asEx s = Literal $ ListL $ Literal . CharL <$> s



interpStringE :: [Op] -> Parser Expr
interpStringE ops = do
    pieces <- lexeme $ interpStr ops
    return $ catAll pieces
    where
        catAll :: [Expr] -> Expr
        catAll [] = Literal $ ListL []
        catAll (e:es) = FCall (FCall (Var "+") e) $ catAll es

recordL :: [Op] -> Parser Lit
recordL ops = fmap RecordL $ braces $ commaSep $ do
    k <- identifier <|> between (char '\"') (char '\"') (many alphaNum)
    symbol ":"
    v <- (expr ops)
    return (k, v)

lambdaL :: [Op] -> Parser Lit
lambdaL ops = do
    symbol "\\"
    ps <- many1 identifier
    reservedOp "->"
    e <- expr ops
    return $ genL ps e
    where
        genL :: [String] -> Expr -> Lit
        genL [p]    e = LambdaL p e
        genL (p:ps) e = LambdaL p (Literal $ genL ps e)
