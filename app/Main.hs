{-# LANGUAGE BlockArguments #-}
module Main where

import Lib
import Parser
import Types
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
                              getDests=getDests state, getFClasses=getFClasses state, getStackTrace=getStackTrace state}

updateArgs :: (M.Map String RTValue -> M.Map String RTValue) -> RTState -> RTState
updateArgs f state = RTState {getVals=getVals state, getArgs=f (getArgs state), getClosures = (getClosures state),
                              getDests=getDests state, getFClasses=getFClasses state, getStackTrace=getStackTrace state}

updateClosures :: (M.Map String RTValue -> M.Map String RTValue) -> RTState -> RTState
updateClosures f state = RTState {getVals=getVals state, getArgs= (getArgs state), getClosures =f (getClosures state),
                                  getDests=getDests state, getFClasses=getFClasses state, getStackTrace=getStackTrace state}

updateDests :: ([Destr] -> [Destr]) -> RTState -> RTState
updateDests f state = RTState {getVals=getVals state, getArgs=getArgs state, getClosures=getClosures state,
                               getDests=f (getDests state), getFClasses=getFClasses state, getStackTrace=getStackTrace state}

updateStackTrace :: ([String] -> [String]) -> RTState -> RTState
updateStackTrace f state = RTState {getVals=getVals state, getArgs=getArgs state, getClosures=getClosures state,
                               getDests=getDests state, getFClasses=getFClasses state, getStackTrace=f $ getStackTrace state}

insertFClass :: String -> Int -> FClassInstance -> RTState -> RTState
insertFClass name arity fc state = RTState {getVals=getVals state, getArgs=getArgs state, getClosures=getClosures state,
                                 getDests=getDests state, getFClasses=
                                 M.insertWith ins name (FClassObj arity [fc] []) (getFClasses state), getStackTrace=getStackTrace state}
    where
        ins :: FClassObj -> FClassObj -> FClassObj
        ins (FClassObj a fs cls) (FClassObj _ fs' _) = (FClassObj a (ins' fs fs') cls)
        ins' (x:_) ys = L.insertBy (\(FClassInstance i _) (FClassInstance j _) -> j `compare` i) x ys

emptyState :: RTState
emptyState = RTState {getVals=M.fromList (nativeFs ++ nativeVals), getArgs=M.empty, getClosures=M.empty,
                      getDests=[], getFClasses=M.empty, getStackTrace=[]}


nativeFs :: [(String, RTValue)]
nativeFs = (\(x, y) -> (x, NativeF y M.empty)) <$> [("put", putF), ("debugRaw", debugRawF), ("head", headF),
    ("tail", tailF), ("exec", execF), ("typeof", typeofF), ("eval", evalF), ("pureIO", pureIOF), 
    ("round", roundF), ("showNum", showNumF), ("entries", entriesF), ("debugState", debugStateF), ("toCode", toCodeF)]

nativeVals :: [(String, RTValue)]
nativeVals = [("addNum", addNumF), ("subNum", subNumF), ("ord", ordF), ("mulNum", mulNumF), ("divNum", divNumF), ("cons", consF),
              ("get", getF), ("set", setF), ("compIO", compIOF), ("readLine", readLineF), ("throw", throwF),
              ("rem", remF)]

main :: IO ()
main = do
        fspath <- SE.getEnv "FSPATH" <|> ((++"/.fscript/") <$> SE.getEnv "HOME")
        putStrLn fspath
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
                    state <- if noStdLib then return emptyState else fst <$> runStatement (Import "stdlib/Base" All False) emptyState fspath
                    runAsRepl debugParse noPrint exprOnly state fspath
                False -> runFile filePath debugParse exprOnly noStdLib fspath



runFile :: String -> Bool -> Bool -> Bool -> String -> IO ()
runFile path debugParser exprOnly noStdLib fspath = do
    fileContent <- readFile path
    state <- if noStdLib then return emptyState else fst <$> runStatement (Import "stdlib/Base" All False) emptyState fspath
    let statements = case exprOnly of
            False -> parseFile fileContent path
            True  -> parseFileExpr fileContent path
    case statements of
        Left e -> print e
        Right sts -> runStatements sts state fspath >> return ()


runAsRepl :: Bool -> Bool -> Bool -> RTState -> String -> IO ()
runAsRepl debugParse noPrint exprOnly state fspath = do
    input <- (fromMaybe "") <$> (runInputT (Settings completeFilename (Just $ fspath ++ "history.txt") True) $ (getInputLine "+> "))
    case input of
        "exit" -> return ()
        ('!':cmd) -> callCommand cmd >> runAsRepl debugParse noPrint exprOnly state fspath
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
                            (state', isE) <- runStatements replSTS state fspath
                            return state'
            runAsRepl debugParse noPrint exprOnly state' fspath


statementAsRepl :: Statement -> Statement
statementAsRepl (Run ex) = Run (FCall (Var "printOrExec") ex)
statementAsRepl x = x

runStatements :: [Statement] -> RTState -> String -> IO (RTState, Bool)
runStatements [] state _    = return (state, False)
runStatements (s:sts) state fspath = do
    (state', isException) <- runStatement s state fspath
    if isException then return (state, True)
    else runStatements sts state' fspath

runStatement :: Statement -> RTState -> String -> IO (RTState, Bool)
runStatement NOP                    state _ = return (state, False)
runStatement (Import m iType q)     state fspath = (\x -> (x, False)) <$> runImport m iType q state fspath
runStatement (Def (NormalDef n e))  state _ = return $ (updateVals (M.insert n (eval e state)) state, False)
runStatement (Def (DestDef dn ns e))state _ = case find (\(Destr dn' _ _) -> dn == dn') (getDests state) of
    Nothing -> putStrLn ("Destructuring " ++ dn ++ "does not exist!") >> return (state, True)
    Just (Destr _ v exps) ->
        return $ (updateVals (insertAll ((\(n, ne) -> (n, evalDest v e ne state)) <$> (zip ns exps))) state, False)

runStatement (DefDest dest)         state _ = return $ (updateDests (dest:) state, False)
runStatement (DefFClass name arity fc) state _ = return $ (insertFClass name arity fc state, False)
runStatement (Run e)                state _ = case eval e state of
    (IOV a)                         -> runIOAction a state >> return (state, False)
    (ExceptionV eType eMsg st)      -> (putStrLn $ eType  ++ " Exception: '" ++ eMsg ++ "'" ++ "\n\nStackTrace: " ++ showST st) >> return (state, True)
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


runImport :: String -> ImportType -> Bool -> RTState -> String -> IO RTState
runImport path iType isQualified state fspath = do
    let fileTries = [path, path ++ ".fscript", fspath ++ "modules/" ++ path, fspath ++ "modules/" ++ path ++ ".fscript"]
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
                        All          -> do (state', isE) <- runStatements sts' state fspath
                                           return state'
                        Exposing ids -> let isExposed (Def (NormalDef did _)) = did `elem` (if isQualified then (getImportFromPath path) <$> ids else ids)
                                            --TODO: isExposed (Def (DestDef))
                                            isExposed _ = False
                                        in fst <$> (runStatements (filter isExposed sts') state fspath)

                              --TODO: isDef (Def (DestDef))
                        where isDef (Def (NormalDef _ _)) = True
                              isDef _ = False
                              isDefDest (DefDest _) = True
                              isDefDest _ = False
                              isDefFC (DefFClass _ _ _) = True
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
eval (FCall fx ax)              state = case av of
    ExceptionV eType eMsg st ->            traceIf dEBUG "Exception in av" $ ExceptionV eType eMsg st
    _ -> let state' = updateStackTrace ((toFunName fx):) state in
        case eval fx state' of
            (FuncV pn ex cls)   -> traceIf dEBUG "FCall" eval ex (updateClosures (M.union cls) $ updateArgs (const (M.singleton pn av)) state')
            (NativeF f cls)     -> traceIf dEBUG "FCall NativeF" $ f av (updateClosures (M.union cls) state')
            (FClass n xs cls)   -> traceIf dEBUG "FCall FClass" $ evalFC (FClass n xs cls) cls state' state' av
            ExceptionV et em st -> traceIf dEBUG "Exception" ExceptionV et em st
            x                   -> ExceptionV "Type" ("Tried to call a value that is not a function! The value was '" ++ show x ++ "'") (getStackTrace state)
    where
        toFunName (Var n) = n
        toFunName (Literal (LambdaL _ _)) = "[Lambda]"
        toFunName (If _ _ _) = "[If]"
        toFunName (FCall fe _) = "[FCall(" ++ (toFunName fe) ++ ")]"
        toFunName _ = "[Function]"
        av = eval ax state
        evalFC :: RTValue -> [RTValue] -> RTState -> RTState -> RTValue -> RTValue
        evalFC (FClass _ [] _) icls istate state av = traceIf dEBUG "FCall FClass []" $ ExceptionV "NonExhaustiveFClass" ("FClass cases are not exhaustive!") (getStackTrace state)
        evalFC (FClass 0 ((FClassInstance _ fe):fcs) []) icls istate state av = case eval (FCall fe (Value av)) state
            of
            (ExceptionV "FClass" _ _) -> evalFC (FClass 0 fcs icls) icls istate istate av
            x -> x

        evalFC (FClass 0 ((FClassInstance p fe):fcs) cls) icls istate state av = traceIf dEBUG "FCall FClass" $
            case eval fe state of
                (FuncV pn ex fcls) -> let (a:as) = cls in evalFC (FClass 0 ((FClassInstance p (Value $ eval (FCall fe (Value a)) state)):fcs) as) icls istate (updateClosures (M.insert pn a) state) av

        evalFC (FClass n fcs cls) icls istate state av = (FClass (n - 1) fcs (av:cls))


eval (Let (NormalDef n vx) ex)  state = traceIf dEBUG "Let NormalDef" $ eval ex (updateArgs (M.insert n (eval vx state)) state)
eval (Let (DestDef dn ns e) ex) state = traceIf dEBUG "Let DestDef" $ case find (\(Destr dn' _ _) -> dn == dn') (getDests state) of
    Nothing -> ExceptionV "State" ("Destructuring " ++ dn ++ " does not exist!") (getStackTrace state)
    Just (Destr _ v exps) ->
        eval ex (updateArgs (insertAll ((\(n, ne) -> (n, evalDest v e ne state)) <$> (zip ns exps))) state)
eval (Var n)                 state = traceIf dEBUG ("Var " ++ n) $ case M.lookup n (getArgs state) <|> M.lookup n (getClosures state) <|> M.lookup n (getVals state) <|> ((\(FClassObj n fis cls) -> FClass n fis cls) <$> M.lookup n (getFClasses state)) of
                            Just x  -> x
                            Nothing -> ExceptionV "State" ("Value " ++ n ++ " does not exist in the current state! \n\nCurrent Args were: " ++
                                show (getArgs state) ++ "\n\nClosures were: "
                                ++ show (getClosures state)) (getStackTrace state)

eval (If c th el)               state = traceIf dEBUG "If" $ case (eval c state) of
    BoolV False      -> eval el state
    NumV 0           -> eval el state
    NullV            -> eval el state
    ListV []         -> eval el state
    RecordV []       -> eval el state
    ExceptionV t m s -> ExceptionV t m s
    _                -> eval th state

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
isException (ExceptionV _ _ _) = True
isException _ = False

evalF :: RTValue -> RTState -> RTValue
evalF x state = case rtVAsMStr x of
    Just s -> case parseEval s of
        Left e -> ExceptionV "Parse" (show e) (getStackTrace state)
        Right ex -> eval ex state
    Nothing -> ExceptionV "Type" ("Can only evaluate Strings. '" ++ show x ++ "' is not a String") (getStackTrace state)


showST :: [String] -> String
showST st = if (null st) then "" else foldl1 (\acc -> \cur -> acc ++ " <- " ++ cur) st