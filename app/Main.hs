module Main where

import Lib
import Parser
import qualified System.Environment as SE
import Data.List as L
import Control.Monad
import System.IO
import System.Directory
import System.Process
import Debug.Trace
import Control.Applicative ((<|>))
import Data.Maybe
import Data.Either as E
import Text.Parsec.Error (ParseError)
import Text.Read (readEither)
import System.Console.Haskeline
import Data.Char
import Data.String as S

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
nativeVals = [("add", addF), ("sub", subF), ("ord", ordF), ("mul", mulF), ("cons", consF), ("get", getF), ("set", setF),
              ("compIO", compIOF), ("readLine", readLineF), ("throw", throwF)]

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
                printSplashScreen
                state <- if noStdLib then return emptyState else fst <$> runStatement (Import "stdlib" All False) emptyState
                runAsRepl debugParse noPrint exprOnly state
            False -> runFile filePath debugParse exprOnly noStdLib

printSplashScreen :: IO ()
printSplashScreen = do
                  putStrLn " ____   ____   ______   _______   __   ______   __________"
                  putStrLn "|  __| |  __| |   ___| |   __  | |  | |  __  | |___    ___|"
                  putStrLn "| |__  | |__  |  |     |  |__| | |  | | |__| |     |  |    "
                  putStrLn "|  __| |__  | |  |     |      _| |  | |  ____|     |  |    "
                  putStrLn "| |     __| | |  |___  |  |\\  \\  |  | | |          |  |  "
                  putStrLn "|_|    |____| |______| |__| \\__\\ |__| |_|          |__|    v2.1"


runFile :: String -> Bool -> Bool -> Bool -> IO ()
runFile path debugParser exprOnly noStdLib = do
    fileContent <- readFile path
    state <- if noStdLib then return emptyState else fst <$> runStatement (Import "stdlib" All False) emptyState
    let statements = case exprOnly of
            False -> parseFile fileContent path
            True  -> parseFileExpr fileContent path
    case statements of
        Left e -> print e
        Right sts -> runStatements sts state >> return ()


runAsRepl :: Bool -> Bool -> Bool -> RTState -> IO ()
runAsRepl debugParse noPrint exprOnly state = do
    input <- (fromMaybe "") <$> (runInputT (Settings completeFilename (Just "history.txt") True) $ (getInputLine "+> "))
    case input of
        "exit" -> return ()
        ('!':cmd) -> callCommand cmd >> runAsRepl debugParse noPrint exprOnly state
        _ -> do
            let line = input ++ ";"
            let statements = case exprOnly of
                    False -> parseAsRepl line
                    True  -> parseAsReplExpr line
            state' <- case statements of
                        Left e -> print e >> return state
                        Right sts -> do
                            let replSTS = if noPrint then sts else map statementAsRepl sts
                            when debugParse $ print replSTS
                            (state', isE) <- runStatements replSTS state
                            return state'
            runAsRepl debugParse noPrint exprOnly state'


ioFuncs :: [String]
ioFuncs = ["print", "put", "compIO", "exec"]

statementAsRepl :: Statement -> Statement
statementAsRepl (Run (FCall (Var f) ex))
    | f `elem` ioFuncs = (Run (FCall (Var f) ex))
statementAsRepl (Run ex) = Run (FCall (Var "print") ex)
statementAsRepl x = x

runStatements :: [Statement] -> RTState -> IO (RTState, Bool)
runStatements [] state      = return (state, False)
runStatements (s:sts) state = do
    (state', isException) <- runStatement s state
    if isException then return (state, True)
    else runStatements sts state'

runStatement :: Statement -> RTState -> IO (RTState, Bool)
runStatement NOP                state = return (state, False)
runStatement (Import m iType q) state = (\x -> (x, False)) <$> runImport m iType q state
runStatement (Def n e)          state = return $ (updateVals ((n, eval e state):) state, False)
runStatement (Run e)            state = case eval e state of
    (IOV a)                     -> runIOAction a state >> return (state, False)
    (Exception eType eMsg)      -> (putStrLn $ eType  ++ " Exception: '" ++ eMsg ++ "'") >> return (state, True)
    _                           -> putStrLn "Can only run values of type IO!" >> return (state, False)

runIOAction :: IOAction -> RTState -> IO ()
runIOAction (Print s) state      = putStrLn s
runIOAction (ReadLine) state     = getLine >> return ()
runIOAction (Composed a f) state = case a of
    Print s -> putStrLn s >> tryRun (FCall (Value f) (Value NullV))
    ReadLine -> do
                    line <- getLine
                    let lv = strAsRTV line
                    tryRun $ FCall (Value f) (Value lv)
    where tryRun ex = case eval ex state of
                       IOV a -> runIOAction a state
                       x     -> putStrLn $ "IO actions can only be composed with Functions that return other IO actions. '" ++ show x ++ "is not an IO action"

runImport :: String -> ImportType -> Bool -> RTState -> IO RTState
runImport path iType isQualified state = do
    let file = path ++ ".fscript"
    exists <- doesFileExist file
    case exists of
        False -> putStrLn ("Error! Module at '" ++ file ++ "' does not exist!") >> return state
        True -> do
            content <- readFile file
            case (parseFile content path) of
                Left e -> print e >> return state
                Right sts -> do
                    putStrLn ("Importing module at path '" ++ file ++ "'...")
                    case iType of
                        All          -> do (state', isE) <- runStatements sts' state
                                           return state'
                        Exposing ids -> let isExposed (Def did dex) = did `elem` (if isQualified then ((path++".")++) <$> ids else ids)
                                        in fst <$> (runStatements (filter isExposed sts') state)


                        where isDef (Def did dex) = True
                              isDef _ = False
                              sts' = (\(Def did dex) -> if isQualified then Def (path ++ "." ++ did) dex else Def did dex) <$> filter isDef sts

eval :: Expr -> RTState -> RTValue
eval (Value (NativeF f cls)) state = NativeF f ((getArgs state)++(getClosures state)++cls)
eval (Value val)   state = val
eval (FCall fx ax) state = case av of
    Exception eType eMsg -> Exception eType eMsg
    _ -> case eval fx state of
        (FuncV pn ex cls) -> eval ex (updateClosures (cls++) $ updateArgs (const [(pn, av)]) state)
        (NativeF f cls)   -> f av (updateClosures (cls++) state)
        Exception et em   -> Exception et em
        _                 -> Exception "Type" "Tried to call a value that is not a function!"
    where av = eval ax state

eval (Let n vx ex) state = eval ex (updateArgs ((n, eval vx state):) state)
eval (Var n) state       = case lookup n (getArgs state) <|> lookup n (getClosures state) <|> lookup n (getVals state) of
                            Just x  -> x
                            Nothing -> Exception "State" $ "Value " ++ n ++ " does not exist in the current state! \n\nCurrent Args were: " ++
                                show (getArgs state) ++ "\n\nClosures were: "
                                ++ show (getClosures state)
eval (If c th el) state  = case (eval c state) of
    BoolV False   -> eval el state
    NumV 0        -> eval el state
    NullV         -> eval el state
    ListV []      -> eval el state
    MapV []       -> eval el state
    Exception t m -> Exception t m
    _             -> eval th state

eval (Literal l) state   = case l of
    NumL x       -> NumV x
    CharL x      -> CharV x
    BoolL x      -> BoolV x
    NullL        -> NullV
    ListL xps    -> ListV $ (`eval` state) <$> xps
    LambdaL n xp -> FuncV n xp (getArgs state ++ getClosures state)
    MapL xps     -> MapV $ (\(x, y) -> (x, eval y state)) <$> xps

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
showF (NumV i)          state = strAsRTV $ show i
showF (BoolV b)         state = strAsRTV $ show b
showF (NullV)           state = strAsRTV $ "Null"
showF (NativeF _ _)     state = strAsRTV $ "<Function>"
showF (FuncV _ _ _)     state = strAsRTV $ "<Function>"
showF (IOV _)           state = strAsRTV $ "<IO>"
showF (CharV c)         state = ListV [CharV '\'', CharV c, CharV '\'']
showF (MapV xps)        state = strAsRTV $ "{" ++ intercalate ", " ((\(n, v) -> n ++ ": " ++ (rtVAsStr $ showF v state)) <$> xps) ++ "}"
showF (Exception eT en) state = strAsRTV $ eT ++ " Exception: '" ++ en ++ "'"
showF x                 state = Exception "Type" $ "Cannot call showF on the expression '" ++ show x ++ "'"

showListRT :: [RTValue] -> RTState -> RTValue
showListRT vs state = strAsRTV $ "[" ++ showListRTInner vs ++ "]"
    where
        showListRTInner [] = ""
        showListRTInner (x:[]) = rtVAsStr $ showF x state
        showListRTInner (x:xs) = (rtVAsStr $ showF x state) ++ ", " ++ showListRTInner xs


putF :: RTValue -> RTState -> RTValue
putF (ListV vs) state = IOV $ Print $ rtVAsStr (ListV vs)
putF _ state = Exception "Type" $ "'put' only works with strings. Use 'print' if you want to print other types"

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
            (NumV a,  NumV b)   -> (NumV $ a + b)
            (ListV a, ListV b)  -> (ListV $ a ++ b)
            (BoolV a, BoolV b)  -> (BoolV $ a || b)
            (NullV, x)          -> x
            (x, NullV)          -> x
            (x, y)              -> Exception "Type" $ "Cannot add the expressions '" ++ show x ++ "' and '" ++ show y ++ "'"
            where
                x = fromMaybe (NumV $ -999) $ lookup "x" (getClosures state)

subF :: RTValue
subF = FuncV "x" (Value $ NativeF subFInner []) []
    where
        subFInner :: RTValue -> RTState -> RTValue
        subFInner y state = case (x, y) of
            (NumV a,  NumV b)   -> (NumV $ a - b)
            (ListV a, ListV b)  -> (ListV $ unique a b)
            (BoolV a, BoolV b)  -> (BoolV $ if b then False else a)
            --(NullV, x)          -> NullV
            (x, NullV)          -> x
            (x, y)              -> Exception "Type" $ "Cannot subtract the expressions '" ++ show x ++ "' and '" ++ show y ++ "'"
            where
                x = fromMaybe (NumV $ -999) $ lookup "x" (getClosures state)

eqF :: RTValue
eqF = FuncV "x" (Value $ NativeF eqFInner []) []
    where
        eqFInner :: RTValue -> RTState -> RTValue
        eqFInner y state = BoolV $ x == y
            where
                x = fromMaybe (NumV $ -999) $ lookup "x" (getClosures state)

ordF :: RTValue
ordF = FuncV "x" (Value $ NativeF ordFInner []) []
    where
        ordFInner :: RTValue -> RTState -> RTValue
        ordFInner y state = case (x, y) of
            (NumV a, NumV b)     -> ordToRTInt (compare a b)
            (BoolV a, BoolV b)   -> ordToRTInt (compare a b)
            (CharV a, CharV b)   -> ordToRTInt (compare a b)
            (ListV xs, ListV ys) -> ordToRTInt (case compare (length xs) (length ys) of
                            EQ -> if xs == ys then EQ else LT
                            x -> x)
            (NullV, NullV)       -> NumV 0
            (NullV, _)           -> NumV $ -1
            (_, NullV)           -> NumV 1
            (x, y)               -> Exception "Type" $ "cannot compare the values '" ++ show x ++ "' and '" ++ show y ++
                "' because they are different types and neither of then is 'Null'"
            where
                x = fromMaybe (NumV $ -999) $ lookup "x" (getClosures state)

ordToRTInt :: Ordering -> RTValue
ordToRTInt LT = NumV $ -1
ordToRTInt GT = NumV 1
ordToRTInt EQ = NumV 0

mulF :: RTValue
mulF = FuncV "x" (Value $ NativeF mulFInner []) []
    where
        mulFInner :: RTValue -> RTState -> RTValue
        mulFInner y state = case (x, y) of
            (NumV a,  NumV b)   -> (NumV $ a * b)
            --(ListV a, ListV b)  -> (ListV $ unique a b)
            --(BoolV a, BoolV b)  -> (BoolV $ if b then False else a)
            --(NullV, x)          -> NullV
            --(x, NullV)          -> x
            (x, y)              -> Exception "Type" $ "Cannot multiply the expressions '" ++ show x ++ "' and '" ++ show y ++ "'"
            where
                x = fromMaybe (NumV $ -999) $ lookup "x" (getClosures state)



debugRawF :: RTValue -> RTState -> RTValue
debugRawF x state = strAsRTV $ show x

headF :: RTValue -> RTState -> RTValue
headF x state = case x of
    ListV []    -> NullV
    ListV (x:_) -> x
    x           -> Exception "Type" $ "'head' needs its argument to be a list. '" ++ show x ++ "' is not a List!"

tailF :: RTValue -> RTState -> RTValue
tailF x state = case x of
    ListV []     -> NullV
    ListV (_:xs) -> ListV xs
    x            -> Exception "Type" $ "'tail' needs its argument to be a list. '" ++ show x ++ "' is not a List!"

execF :: RTValue -> RTState -> RTValue
execF (IOV a) state = IOV a
execF x       state = Exception "Type" $ "Can only run 'exec' on values of type IO. '" ++ show x ++ "' does not have the type IO!"

typeofF :: RTValue -> RTState -> RTValue
typeofF (IOV _)         state = strAsRTV "IO"
typeofF (CharV _)       state = strAsRTV "Char"
typeofF (ListV _)       state = strAsRTV "List"
typeofF (NumV _)        state = strAsRTV "Num"
typeofF (BoolV _)       state = strAsRTV "Bool"
typeofF (FuncV _ _ _)   state = strAsRTV "Function"
typeofF (NativeF _ _)   state = strAsRTV "Function"
typeofF (NullV)         state = strAsRTV "Null"
typeofF (MapV _)        state = strAsRTV "Map"
typeofF (Exception _ _) state = strAsRTV "Exception"

consF :: RTValue
consF = FuncV "x" (Value $ NativeF consInner []) []
    where
        consInner :: RTValue -> RTState -> RTValue
        consInner xs state = case xs of
            ListV l -> ListV (x:l)
            x       -> Exception "Type" $ "cons needs its second argument to be of type List. The value '" ++ show x ++ "' is not a List!"
            where
                x = fromMaybe (NumV $ -999) $ lookup "x" (getClosures state)


getF :: RTValue
getF = FuncV "n" (Value $ NativeF getInner []) []
    where
        getInner :: RTValue -> RTState -> RTValue
        getInner ms state = case ms of
            MapV m -> case rtVAsMStr n of
                Nothing -> Exception "Type" $ "get needs its first argument to be of type String. The value '" ++ show n ++ "' is not a String!"
                Just s  -> fromMaybe NullV $ lookup s m
            x -> Exception "Type" $ "get needs its second argument to be of type Map. The value '" ++ show x ++ "' is not a Map!"
            where
                n = fromMaybe (NumV $ -999) $ lookup "n" (getClosures state)

setF :: RTValue
setF = FuncV "n" (Literal (LambdaL "x" (Value $ NativeF setInner []))) []
    where
        setInner :: RTValue -> RTState -> RTValue
        setInner ms state = case ms of
            MapV m -> case rtVAsMStr n of
                Nothing -> Exception "Type" $ "set needs its first argument to be of type String. The value '" ++ show n ++ "' is not a String!"
                Just s  -> MapV $ setAL s x m
            x -> Exception "Type" $ "set needs its second argument to be of type Map. The value '" ++ show x ++ "' is not a Map!"
            where
                n = fromMaybe (NumV $ -999) $ lookup "n" (getClosures state)
                x = fromMaybe (NumV $ -777) $ lookup "x" (getClosures state)


setAL :: (Eq k) => k -> v -> [(k, v)] -> [(k, v)]
setAL k v []     = [(k, v)]
setAL k v ((k', v'):xs)
    | k == k'    = ((k, v):xs)
    | otherwise  = ((k', v'):(setAL k v xs))


compIOF :: RTValue
compIOF = FuncV "io" (Value $ NativeF compInner []) []
    where
        compInner :: RTValue -> RTState -> RTValue
        compInner f state = case io of
            IOV a -> IOV $ Composed a f
            x -> Exception "Type" $ "compIO needs its first argument to be an IO action. '" ++ show x ++ "' is not an IO action!"
            where
                io = fromMaybe (NumV $ -999) $ lookup "io" (getClosures state)

readLineF :: RTValue
readLineF = IOV ReadLine

throwF :: RTValue
throwF = FuncV "type" (Value $ NativeF throwInner []) []
    where
        throwInner :: RTValue -> RTState -> RTValue
        throwInner name state = Exception (rtVAsMStr typen |> fromMaybe "Unknown") (rtVAsMStr name |> fromMaybe "Unknown")
            where
                typen = fromMaybe (NumV $ -999) $ lookup "type" (getClosures state)


(|>) :: a -> (a -> b) -> b
x |> f = f x
