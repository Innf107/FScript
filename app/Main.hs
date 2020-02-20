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
import Control.Applicative
import Data.Maybe
import Data.Either as E
import Text.Parsec.Error (ParseError)
import Text.Read (readEither)
import System.Console.Haskeline
import Data.Char
import Data.String as S
import Data.List.Split
import Cmdline
import NativeFs
import System.Console.ANSI
import Control.Parallel.Strategies
import qualified Data.Map as M

updateVals :: (M.Map String RTValue -> M.Map String RTValue) -> RTState -> RTState
updateVals f state = RTState {getVals=f(getVals state), getArgs=getArgs state, getClosures = (getClosures state),
                              getDests=getDests state, getFClasses=getFClasses state}

updateArgs :: (M.Map String RTValue -> M.Map String RTValue) -> RTState -> RTState
updateArgs f state = RTState {getVals=getVals state, getArgs=f (getArgs state), getClosures = (getClosures state),
                              getDests=getDests state, getFClasses=getFClasses state}

updateClosures :: (M.Map String RTValue -> M.Map String RTValue) -> RTState -> RTState
updateClosures f state = RTState {getVals=getVals state, getArgs= (getArgs state), getClosures =f (getClosures state),
                                  getDests=getDests state, getFClasses=getFClasses state}

updateDests :: ([Destr] -> [Destr]) -> RTState -> RTState
updateDests f state = RTState {getVals=getVals state, getArgs=getArgs state, getClosures=getClosures state,
                               getDests=f (getDests state), getFClasses=getFClasses state}

insertFClass :: String -> FClassInstance -> RTState -> RTState
insertFClass name fc state = RTState {getVals=getVals state, getArgs=getArgs state, getClosures=getClosures state,
                                 getDests=getDests state, getFClasses=
                                 M.insertWith ins name [fc] (getFClasses state)}
    where
        ins :: [FClassInstance] -> [FClassInstance] -> [FClassInstance]
        ins (x:_) ys = L.insertBy (\(FClassInstance i _) (FClassInstance j _) -> j `compare` i) x ys

emptyState :: RTState
emptyState = RTState {getVals=M.fromList (nativeFs ++ nativeVals), getArgs=M.empty, getClosures=M.empty,
                      getDests=[], getFClasses=M.empty}


nativeFs :: [(String, RTValue)]
nativeFs = (\(x, y) -> (x, NativeF y M.empty)) <$> [("put", putF), ("debugRaw", debugRawF), ("head", headF),
    ("tail", tailF), ("exec", execF), ("typeof", typeofF), ("eval", evalF), ("pureIO", pureIOF), 
    ("round", roundF), ("showNum", showNumF), ("entries", entriesF)]

nativeVals :: [(String, RTValue)]
nativeVals = [("add", addF), ("sub", subF), ("ord", ordF), ("mul", mulF), ("div", divF), ("cons", consF),
              ("get", getF), ("set", setF), ("compIO", compIOF), ("readLine", readLineF), ("throw", throwF),
              ("rem", remF)]

main :: IO ()
main = do
        args <- SE.getArgs
        let repl = "--repl" `elem` args || "-r" `elem` args
        let debugParse = "--debug-parse" `elem` args
        let exprOnly = "--debug-expr-only" `elem` args
        let noPrint = "--debug-stmnt-only" `elem` args
        let noStdLib = "--debug-no-stdlib" `elem` args
        let help = "--help" `elem` args || "-h" `elem` args || (length args) == 0
        let monochrome = "--monochrome" `elem` args || "-m" `elem` args
        if help then showHelp else
            let filePath = args!!0 in
            case repl of
                True -> do
                    printSplashScreen
                    when (not monochrome) $ setSGR [SetColor Foreground Vivid Cyan]
                    state <- if noStdLib then return emptyState else fst <$> runStatement (Import "stdlib/Base" All False) emptyState
                    runAsRepl debugParse noPrint exprOnly state
                False -> runFile filePath debugParse exprOnly noStdLib



runFile :: String -> Bool -> Bool -> Bool -> IO ()
runFile path debugParser exprOnly noStdLib = do
    fileContent <- readFile path
    state <- if noStdLib then return emptyState else fst <$> runStatement (Import "stdlib/Base" All False) emptyState
    let statements = case exprOnly of
            False -> parseFile fileContent path
            True  -> parseFileExpr fileContent path
    case statements of
        Left e -> print e
        Right sts -> runStatements sts state >> return ()


runAsRepl :: Bool -> Bool -> Bool -> RTState -> IO ()
runAsRepl debugParse noPrint exprOnly state = do
    input <- (fromMaybe "") <$> (runInputT (Settings completeFilename (Just $ fscriptDir ++ "history.txt") True) $ (getInputLine "+> "))
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


statementAsRepl :: Statement -> Statement
statementAsRepl (Run ex) = Run (FCall (Var "printOrExec") ex)
statementAsRepl x = x

runStatements :: [Statement] -> RTState -> IO (RTState, Bool)
runStatements [] state      = return (state, False)
runStatements (s:sts) state = do
    (state', isException) <- runStatement s state
    if isException then return (state, True)
    else runStatements sts state'

runStatement :: Statement -> RTState -> IO (RTState, Bool)
runStatement NOP                    state = return (state, False)
runStatement (Import m iType q)     state = (\x -> (x, False)) <$> runImport m iType q state
runStatement (Def (NormalDef n e))  state = return $ (updateVals (M.insert n (eval e state)) state, False)
runStatement (Def (DestDef dn ns e))state = case find (\(Destr dn' _ _) -> dn == dn') (getDests state) of
    Nothing -> putStrLn ("Destructuring " ++ dn ++ "does not exist!") >> return (state, True)
    Just (Destr _ v exps) ->
        return $ (updateVals (insertAll ((\(n, ne) -> (n, evalDest v e ne state)) <$> (zip ns exps))) state, False)

runStatement (DefDest dest)         state = return $ (updateDests (dest:) state, False)
runStatement (DefFClass name fc)       state = return $ (insertFClass name fc state, False)
runStatement (Run e)                state = case eval e state of
    (IOV a)                         -> runIOAction a state >> return (state, False)
    (ExceptionV eType eMsg)          -> (putStrLn $ eType  ++ " Exception: '" ++ eMsg ++ "'") >> return (state, True)
    _                               -> putStrLn "Can only run values of type IO!" >> return (state, True)

evalDest :: String -> Expr -> Expr -> RTState -> RTValue
evalDest v e ne state = eval (Let (NormalDef v e) ne) state


runIOAction :: IOAction -> RTState -> IO ()
runIOAction (Print s) state      = putStrLn s
runIOAction (ReadLine) state     = getLine >> return ()
runIOAction (ReadFile _) state   = putStrLn "The IOAction ReadFile is useless unless it is used with compIO"
runIOAction (PureIO _) state     = return ()
runIOAction (Composed a f) state = case a of
    Print s -> putStrLn s >> tryRun (FCall (Value f) (Value NullV))
    ReadLine -> readBasic (strAsRTV <$> getLine)
    ReadFile fp -> readBasic (strAsRTV <$> readFile fp)
    PureIO v -> readBasic (return v)
    where readBasic io = io >>= (\c -> tryRun $ FCall (Value f) (Value c))
          tryRun ex = case eval ex state of
                       IOV a -> runIOAction a state
                       x     -> putStrLn $ "IO actions can only be composed with Functions that return other IO actions. '" ++ show x ++ "is not an IO action"


fscriptDir = "/etc/fscript/"
stdlibDir = fscriptDir ++ "modules/"

runImport :: String -> ImportType -> Bool -> RTState -> IO RTState
runImport path iType isQualified state = do
    let fileTries = [path, path ++ ".fscript", stdlibDir ++ path, stdlibDir ++ path ++ ".fscript"]
    fileM <- findM doesFileExist fileTries
    case fileM of
        Nothing -> putStrLn ("Error! Module '" ++ path ++ "' does not exist!") >> return state
        Just file -> do
            content <- readFile file
            case (parseFile content path) of
                Left e -> print e >> return state
                Right sts -> do
                    putStrLn ("Importing module at path '" ++ file ++ "'...")
                    case iType of
                        All          -> do (state', isE) <- runStatements sts' state
                                           return state'
                        Exposing ids -> let isExposed (Def (NormalDef did _)) = did `elem` (if isQualified then (getImportFromPath path) <$> ids else ids)
                                            --TODO: isExposed (Def (DestDef))
                                            isExposed _ = False
                                        in fst <$> (runStatements (filter isExposed sts') state)

                              --TODO: isDef (Def (DestDef))
                        where isDef (Def (NormalDef _ _)) = True
                              isDef _ = False
                              isDefDest (DefDest _) = True
                              isDefDest _ = False
                              isDefFC (DefFClass _ _) = True
                              isDefFC _ = False
                              qF (Def (NormalDef did dex)) = if isQualified then Def $ NormalDef (getImportFromPath path did) dex else Def $ NormalDef did dex
                              --TODO: qF (Def (DestDef))
                              sts' = (qF <$> filter isDef sts) ++ filter isDefDest sts ++ filter isDefFC sts

getImportFromPath :: String -> String -> String
getImportFromPath path did = (last $ endBy "/" path) ++ "." ++ did

dEBUG = False

eval :: Expr -> RTState -> RTValue
eval (Value (NativeF f cls))    state = traceIf dEBUG "Value NativeF" $ NativeF f (M.unions [(getArgs state), (getClosures state),cls])
eval (Value val)                state = traceIf dEBUG ("Value " ++ show val) $ val
eval (FCall fx ax)              state = traceIf dEBUG "FCallStart" $ case av of
    ExceptionV eType eMsg ->            traceIf dEBUG "Exception in av" $ ExceptionV eType eMsg
    _ -> case eval fx state of
        (FuncV pn ex cls) -> traceIf dEBUG "FCall" eval ex (updateClosures (M.union cls) $ updateArgs (const (M.singleton pn av)) state)
        (NativeF f cls)   -> traceIf dEBUG "FCall NativeF" $ f av (updateClosures (M.union cls) state)
        (FClass [])       -> traceIf dEBUG "FCall FClass []" $ ExceptionV "NonExhaustiveFClass" $ "FClass cases are not exhaustive!"
        (FClass ((FClassInstance _ fe):fcs)) -> traceIf dEBUG "FCall FClass" $
                case eval (FCall fe ax) state of
                (ExceptionV "Type" _) -> eval (FCall (Value $ FClass fcs) ax) state
                x -> x
        ExceptionV et em  -> traceIf dEBUG "Exception" ExceptionV et em
        x                 -> ExceptionV "Type" $ "Tried to call a value that is not a function! The value was '" ++ show x ++ "'"
    where av = eval ax state

eval (Let (NormalDef n vx) ex)  state = traceIf dEBUG "Let NormalDef" $ eval ex (updateArgs (M.insert n (eval vx state)) state)
eval (Let (DestDef dn ns e) ex) state = traceIf dEBUG "Let DestDef" $ case find (\(Destr dn' _ _) -> dn == dn') (getDests state) of
    Nothing -> ExceptionV "State" ("Destructuring " ++ dn ++ " does not exist!")
    Just (Destr _ v exps) ->
        eval ex (updateArgs (insertAll ((\(n, ne) -> (n, evalDest v e ne state)) <$> (zip ns exps))) state)
eval (Var n)                    state = traceIf dEBUG ("Var " ++ n) $ case M.lookup n (getArgs state) <|> M.lookup n (getClosures state) <|> M.lookup n (getVals state) <|> (FClass <$> M.lookup n (getFClasses state)) of
                            Just x  -> x
                            Nothing -> ExceptionV "State" $ "Value " ++ n ++ " does not exist in the current state! \n\nCurrent Args were: " ++
                                show (getArgs state) ++ "\n\nClosures were: "
                                ++ show (getClosures state)
eval (If c th el)               state = traceIf dEBUG "If" $ case (eval c state) of
    BoolV False   -> eval el state
    NumV 0        -> eval el state
    NullV         -> eval el state
    ListV []      -> eval el state
    RecordV []       -> eval el state
    ExceptionV t m -> ExceptionV t m
    _             -> eval th state

eval (Literal l)                state = traceIf dEBUG ("Literal " ++ show l) $ case l of
    NumL x       -> NumV x
    CharL x      -> CharV x
    BoolL x      -> BoolV x
    NullL        -> NullV
    ListL xps    -> case ex of
        Just e  -> e
        Nothing -> ListV $ args
        where args = ((`eval` state) <$> xps)
              ex = find isException args
    LambdaL n xp -> FuncV n xp (M.union (getArgs state) (getClosures state))
    RecordL xps     -> case ex of
        Just e -> snd e
        Nothing -> RecordV args
        where args = (\(x, y) -> (x, eval y state)) <$> xps
              ex = find (isException . snd) args

isException :: RTValue -> Bool
isException (ExceptionV _ _) = True
isException _ = False

evalF :: RTValue -> RTState -> RTValue
evalF x state = case rtVAsMStr x of
    Just s -> case parseEval s of
        Left e -> ExceptionV "Parse" $ show e
        Right ex -> eval ex state
    Nothing -> ExceptionV "Type" $ "Can only evaluate Strings. '" ++ show x ++ "' is not a String"
