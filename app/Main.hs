{-# LANGUAGE FlexibleInstances #-}
module Main where

import Lib
import Parser
import qualified System.Environment as SE
import Data.List as L
import Control.Monad
import System.IO
import System.Directory
import Debug.Trace
import Control.Applicative ((<|>))
import Data.Maybe
import Data.Either as E
import Text.Parsec.Error (ParseError)
import Text.Read (readEither)
import System.Console.Haskeline
import Data.Char (ord)

updateVals :: ([(String, RTValue)] -> [(String, RTValue)]) -> RTState -> RTState
updateVals f state = RTState {getVals=f(getVals state), getArgs=getArgs state, getClosures = (getClosures state)}

updateArgs :: ([(String, RTValue)] -> [(String, RTValue)]) -> RTState -> RTState
updateArgs f state = RTState {getVals=getVals state, getArgs=f (getArgs state), getClosures = (getClosures state)}

updateClosures :: ([(String, RTValue)] -> [(String, RTValue)]) -> RTState -> RTState
updateClosures f state = RTState {getVals=getVals state, getArgs= (getArgs state), getClosures =f (getClosures state)}


emptyState :: RTState
emptyState = RTState {getVals=nativeFs ++ nativeVals, getArgs=[], getClosures=[]}


nativeFs :: [(String, RTValue)]
nativeFs = (\(x, y) -> (x, NativeF y [])) <$> [("put", putF), ("show", showF), ("debugRaw", debugRawF), ("head", headF),
    ("tail", tailF), ("exec", execF), ("typeof", typeofF)]

nativeVals :: [(String, RTValue)]
nativeVals = [("add", addF), ("sub", subF), ("ord", ordF), ("mul", mulF), ("cons", consF)]

main :: IO ()
main = do
        args <- SE.getArgs
        let filePath = args!!0
        let repl = "--repl" `elem` args
        let debugParse = "--debug-parse" `elem` args
        let exprOnly = "--debug-expr-only" `elem` args
        let noPrint = "--debug-stmnt-only" `elem` args
        let noStdLib = "--debug-no-stdlib" `elem` args
        case repl of
            True -> do
                putStrLn "Running FScript v2.0 in Repl mode."
                state <- if noStdLib then return emptyState else runStatement (Import "stdlib") emptyState
                runAsRepl debugParse noPrint exprOnly state
            False -> runFile filePath debugParse exprOnly noStdLib

runFile :: String -> Bool -> Bool -> Bool -> IO ()
runFile path debugParser exprOnly noStdLib = do
    fileContent <- readFile path
    state <- if noStdLib then return emptyState else runStatement (Import "stdlib") emptyState
    let statements = case exprOnly of
            False -> parseFile fileContent path
            True  -> parseFileExpr fileContent path
    case statements of
        Left e -> print e
        Right sts -> runStatements sts state >> putStrLn "exiting!"

runAsRepl :: Bool -> Bool -> Bool -> RTState -> IO ()
runAsRepl debugParse noPrint exprOnly state = do
    line <- ((++";") . fromMaybe "") <$> (runInputT defaultSettings $ getInputLine "+> ")
    let statements = case exprOnly of
            False -> parseAsRepl line
            True  -> parseAsReplExpr line
    state' <- case statements of
                Left e -> print e >> return state
                Right sts -> do
                    let replSTS = if noPrint then sts else map statementAsRepl sts
                    when debugParse $ print replSTS
                    runStatements replSTS state
    runAsRepl debugParse noPrint exprOnly state'

ioFuncs :: [String]
ioFuncs = ["print", "put"]

statementAsRepl :: Statement -> Statement
statementAsRepl (Run (FCall (Var f) ex))
    | f `elem` ioFuncs = (Run (FCall (Var f) ex))
statementAsRepl (Run ex) = Run (FCall (Var "print") ex)
statementAsRepl x = x

runStatements :: [Statement] -> RTState -> IO RTState
runStatements [] state      = return state
runStatements (s:sts) state = do
    state' <- runStatement s state
    runStatements sts state'

runStatement :: Statement -> RTState -> IO RTState
runStatement NOP state = return state
runStatement (Import m) state = runImport m state
runStatement (Def n e) state  = return $ updateVals ((n, eval e state):) state
runStatement (Run e) state = case eval e state of
    (IOV a) -> runIOAction a >> return state
    _       -> putStrLn "Can only run values of type IO!" >> return state

runIOAction :: IOAction -> IO ()
runIOAction (Print s) = putStrLn s

runImport :: String -> RTState -> IO RTState
runImport path state = do
    let file = path ++ ".fscript"
    exists <- doesFileExist file
    case exists of
        False -> putStrLn ("Error! Module at '" ++ file ++ "' does not exist!") >> return state
        True -> do
            content <- readFile file
            case (parseFile content path) of
                Left e -> print e >> return state
                Right sts -> putStrLn ("Importing module at path '" ++ file ++ "'...") >> runStatements sts state


eval :: Expr -> RTState -> RTValue
eval (Value (NativeF f cls)) state = NativeF f ((getArgs state)++cls)
eval (Value val)   state = val
eval (FCall fx ax) state = case eval fx state of
        (FuncV pn ex cls) -> eval ex (updateClosures (cls++) $ updateArgs (const [(pn, eval ax state)]) state)
        (NativeF f cls)   -> f (eval ax state) (updateClosures (cls++) state)
        _                 -> error "Tried to call a value that is not a function!"
eval (Let n vx ex) state = eval ex (updateArgs ((n, eval vx state):) state)
eval (Var n) state       = case lookup n (getVals state) <|> lookup n (getArgs state) <|> lookup n (getClosures state) of
                            Just x  -> x
                            Nothing -> error $ "Value " ++ n ++ " does not exist in the current state! \n\nCurrent Args were: " ++ 
                                show (getArgs state) ++ "\n\nClosures were: "
                                ++ show (getClosures state)
eval (If c th el) state  = case (eval c state) of
    BoolV False -> eval el state
    IntV 0      -> eval el state
    NullV       -> eval el state
    ListV []    -> eval el state
    _           -> eval th state

eval (Literal l) state   = case l of
    IntL x       -> IntV x
    CharL x      -> CharV x
    BoolL x      -> BoolV x
    NullL        -> NullV
    ListL xps    -> ListV $ (`eval` state) <$> xps
    LambdaL n xp -> FuncV n xp (getArgs state ++ getClosures state)


-- f = \x -> \y -> x
-- FuncV "x" (Literal (LambdaL "y" (Var "x"))) []

-- f 3
-- FuncV "y" (Var "x") [("x",IntV 3)]

-- g = \x -> y
-- FuncV "x" (Var "y") []

-- h = \y -> g 3
-- FuncV "y" (FCall (Var "g") (Literal (IntL 3))) []

showF :: RTValue -> RTState -> RTValue
showF (ListV vs)    state = case rtVAsMStr (ListV vs) of
                          Just s -> (ListV $ map CharV $ '"':s++['"'])
                          Nothing -> showListRT vs state
showF (IntV i)      state = strAsRTV $ show i
showF (BoolV b)     state = strAsRTV $ show b
showF (NullV)       state = strAsRTV $ "Null"
showF (NativeF _ _) state = strAsRTV $ "<Function>"
showF (FuncV _ _ _) state = strAsRTV $ "<Function>"
showF (IOV _)       state = strAsRTV $ "<IO>"
showF (CharV c)     state = ListV [CharV '\'', CharV c, CharV '\'']

showListRT :: [RTValue] -> RTState -> RTValue
showListRT vs state = strAsRTV $ "[" ++ showListRTInner vs ++ "]"
    where
        showListRTInner [] = ""
        showListRTInner (x:[]) = rtVAsStr $ showF x state
        showListRTInner (x:xs) = (rtVAsStr $ showF x state) ++ ", " ++ showListRTInner xs


putF :: RTValue -> RTState -> RTValue
putF (ListV vs) state = IOV $ Print $ rtVAsStr (ListV vs)
putF _ state = error $ "'put' only works with strings. Use 'print' if you want to print other types"

rtVAsStr :: RTValue -> String
rtVAsStr (ListV vs) = map rtVAsChar vs
rtVAsStr x = error $ "Provided value was NOT a valid string! It was: " ++ show x

rtVAsMStr :: RTValue -> Maybe String
rtVAsMStr (ListV vs) = sequenceA $ map rtVAsMChar vs
rtVAsMStr x = Nothing

rtVAsChar :: RTValue -> Char
rtVAsChar (CharV c) = c
rtVAsChar x = error $ "Provided value was NOT a valid char! It was: " ++ show x

rtVAsMChar :: RTValue -> Maybe Char
rtVAsMChar (CharV c) = Just c
rtVAsMChar x = Nothing

strAsRTV :: String -> RTValue
strAsRTV s = ListV $ map CharV s

addF :: RTValue
addF = FuncV "x" (Value $ NativeF addFInner []) []
    where
        addFInner :: RTValue -> RTState -> RTValue
        addFInner y state = case (x, y) of
            (IntV a,  IntV b)   -> (IntV $ a + b)
            (ListV a, ListV b)  -> (ListV $ a ++ b)
            (BoolV a, BoolV b)  -> (BoolV $ a || b)
            (NullV, x)          -> x
            (x, NullV)          -> x
            (x, y)              -> error $ "Cannot add the expressions '" ++ show x ++ "' and '" ++ show y ++ "'"
            where
                x = fromMaybe (IntV $ -999) $ lookup "x" (getClosures state)

subF :: RTValue
subF = FuncV "x" (Value $ NativeF subFInner []) []
    where
        subFInner :: RTValue -> RTState -> RTValue
        subFInner y state = case (x, y) of
            (IntV a,  IntV b)   -> (IntV $ a - b)
            (ListV a, ListV b)  -> (ListV $ unique a b)
            (BoolV a, BoolV b)  -> (BoolV $ if b then False else a)
            --(NullV, x)          -> NullV
            (x, NullV)          -> x
            (x, y)              -> error $ "Cannot subtract the expressions '" ++ show x ++ "' and '" ++ show y ++ "'"
            where
                x = fromMaybe (IntV $ -999) $ lookup "x" (getClosures state)

eqF :: RTValue
eqF = FuncV "x" (Value $ NativeF eqFInner []) []
    where
        eqFInner :: RTValue -> RTState -> RTValue
        eqFInner y state = BoolV $ x == y
            where
                x = fromMaybe (IntV $ -999) $ lookup "x" (getClosures state)

ordF :: RTValue
ordF = FuncV "x" (Value $ NativeF ordFInner []) []
    where
        ordFInner :: RTValue -> RTState -> RTValue
        ordFInner y state = case (x, y) of
            (IntV a, IntV b)     -> ordToRTInt (compare a b)
            (BoolV a, BoolV b)   -> ordToRTInt (compare a b)
            (CharV a, CharV b)   -> ordToRTInt (compare a b)
            (ListV xs, ListV ys) -> ordToRTInt (compare (length xs) (length ys))
            (NullV, NullV)       -> IntV 0
            (NullV, _)           -> IntV $ -1
            (_, NullV)           -> IntV 1
            (x, y)               -> error $ "cannot compare the values '" ++ show x ++ "' and '" ++ show y ++
                "' because they are different types and neither of then is 'Null'"
            where
                x = fromMaybe (IntV $ -999) $ lookup "x" (getClosures state)

ordToRTInt :: Ordering -> RTValue
ordToRTInt LT = IntV $ -1
ordToRTInt GT = IntV 1
ordToRTInt EQ = IntV 0

mulF :: RTValue
mulF = FuncV "x" (Value $ NativeF mulFInner []) []
    where
        mulFInner :: RTValue -> RTState -> RTValue
        mulFInner y state = case (x, y) of
            (IntV a,  IntV b)   -> (IntV $ a * b)
            --(ListV a, ListV b)  -> (ListV $ unique a b)
            --(BoolV a, BoolV b)  -> (BoolV $ if b then False else a)
            --(NullV, x)          -> NullV
            --(x, NullV)          -> x
            (x, y)              -> error $ "Cannot multiply the expressions '" ++ show x ++ "' and '" ++ show y ++ "'"
            where
                x = fromMaybe (IntV $ -999) $ lookup "x" (getClosures state)



debugRawF :: RTValue -> RTState -> RTValue
debugRawF x state = strAsRTV $ show x

headF :: RTValue -> RTState -> RTValue
headF x state = case x of
    ListV []    -> NullV
    ListV (x:_) -> x
    x           -> error $ "'head' needs its argument to be a list. '" ++ show x ++ "' is not a List!"

tailF :: RTValue -> RTState -> RTValue
tailF x state = case x of
    ListV []     -> NullV
    ListV (_:xs) -> ListV xs
    x            -> error $ "'tail' needs its argument to be a list. '" ++ show x ++ "' is not a List!"

execF :: RTValue -> RTState -> RTValue
execF (IOV a) state = IOV a
execF x       state = error $ "Can only run 'exec' on values of type IO. '" ++ show x ++ "' does not have the type IO!"

typeofF :: RTValue -> RTState -> RTValue
typeofF (IOV _)       state = strAsRTV "IO"
typeofF (CharV _)     state = strAsRTV "Char"
typeofF (ListV _)     state = strAsRTV "List"
typeofF (IntV _)      state = strAsRTV "Int"
typeofF (BoolV _)     state = strAsRTV "Bool"
typeofF (FuncV _ _ _) state = strAsRTV "Function"
typeofF (NativeF _ _) state = strAsRTV "Function"
typeofF (NullV)       state = strAsRTV "Null"

consF :: RTValue
consF = FuncV "x" (Value $ NativeF consInner []) []
    where
        consInner :: RTValue -> RTState -> RTValue
        consInner xs state = case xs of
            ListV l -> ListV (x:l)
            x       -> error $ "cons needs its second argument to be of type List. The value '" ++ show x ++ "' is not a List!"
            where
                x = fromMaybe (IntV $ -999) $ lookup "x" (getClosures state)

