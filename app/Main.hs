{-# LANGUAGE BlockArguments #-}
module Main where
--Test?
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
import Data.Bifunctor
import Control.Monad.State
import qualified Data.Map as M
import Control.Monad.State.Lazy (State(..))
import Control.Monad.Identity (Identity(..))


updateVals :: (M.Map String Variable -> M.Map String Variable) -> RTState -> RTState
updateVals f state = state {getVals=f(getVals state)}

updateVal :: (Variable -> Variable) -> String -> RTState -> RTState
updateVal f k state = state {getVals=M.adjust f k $ getVals state, getArgs=M.adjust f k $ getArgs state, getClosures = M.adjust f k $ getClosures state}

updateArgs :: (M.Map String Variable -> M.Map String Variable) -> RTState -> RTState
updateArgs f state = state {getArgs=f (getArgs state)}

updateClosures :: (M.Map String Variable -> M.Map String Variable) -> RTState -> RTState
updateClosures f state = state {getClosures=f (getClosures state)}

updateDests :: ([Destr] -> [Destr]) -> RTState -> RTState
updateDests f state = state {getDests=f (getDests state)}

updateStackTrace :: ([String] -> [String]) -> RTState -> RTState
updateStackTrace f state = state {getStackTrace=f $ getStackTrace state}

insertFClass :: String -> Int -> FClassInstance -> RTState -> RTState
insertFClass name arity fc state = state {getFClasses=
                                 M.insertWith ins name (FClassObj arity [fc] []) (getFClasses state)}
    where
        ins :: FClassObj -> FClassObj -> FClassObj
        ins (FClassObj a fs cls) (FClassObj _ fs' _) = (FClassObj a (ins' fs fs') cls)
        ins' (x:_) ys = L.insertBy (\(FClassInstance i _) (FClassInstance j _) -> j `compare` i) x ys

emptyState :: RTState
emptyState = RTState {getVals=Eager <$> M.fromList (nativeFs ++ nativeVals), getArgs=M.empty, getClosures=M.empty,
                      getDests=[], getFClasses=M.empty, getStackTrace=[], getEvaledVals=M.empty, getEvaledArgs=M.empty, 
                      getEvaledCls=M.empty}


nativeFs :: [(String, RTValue)]
nativeFs = (\(x, y) -> (x, NativeF (y evalVar) M.empty)) <$> [("put", putF), ("debugRaw", debugRawF), ("head", headF),
    ("tail", tailF), ("typeof", typeofF), ("eval", evalF), ("pureIO", pureIOF), ("sys", sysF),
    ("round", roundF), ("showNum", showNumF), ("entries", entriesF), ("debugState", debugStateF), ("toCode", toCodeF),
    ("debugFClass", debugFClassF), ("debugFClasses", debugFClassesF)]

nativeVals :: [(String, RTValue)]
nativeVals = (second (\f -> f evalVar)) <$> [("addNum", addNumF), ("subNum", subNumF), ("ord", ordF), ("mulNum", mulNumF),
              ("divNum", divNumF), ("cons", consF), ("get", getF), ("set", setF), ("compIO", compIOF), ("readLine", readLineF),
              ("throw", throwF), ("error", errorF), ("rem", remF)]

main :: IO ()
main = do
        fspath <- SE.getEnv "FSPATH" <|> ((++"/.fscript/") <$> SE.getEnv "HOME")
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
                    False -> parseAsRepl [] line
                    True  -> (\x -> (x, [])) <$> (parseAsReplExpr line)
            state' <- case statements of
                        Left e -> print e >> return state
                        Right (sts, ops) -> do
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
runStatement (Def (NormalDef n e))  state _ = return $ (updateVals (M.insert n (Lazy e)) state, False)
runStatement (Def (DestDef dn ns e))state _ = case find (\(Destr dn' _ _) -> dn == dn') (getDests state) of
    Nothing -> putStrLn ("Destructuring " ++ dn ++ "does not exist!") >> return (state, True)
    Just (Destr _ v exps) ->
        return $ (updateVals (insertAll ((\(n, ne) -> (n, Lazy $ dest2Eval v e ne state)) <$> (zip ns exps))) state, False)

runStatement (DefDest dest)         state _ = return $ (updateDests (dest:) state, False)
runStatement (DefFClass name arity fc) state _ = return $ (insertFClass name arity fc state, False)
runStatement (Run e)                state _ = let (x, state') = runState (eval e) state in case x of
    (IOV a)                         -> runIOAction a state' >> return (state', False)
    (ExceptionV eType eMsg ed st)      -> (putStrLn $ eType  ++ " Exception: '" ++ eMsg ++ "'\nData: '" ++ fromMaybe "UNKNOWN TYPE" (rtVAsMStr $ evalfst' (FCall (Var "show") (Value (Eager ed))) state) ++ "'\n\nStackTrace: " ++ showST st) >> return (state', True)
    _                               -> putStrLn "Can only run values of type IO!" >> return (state', True)



runIOAction :: IOAction -> RTState -> IO ()
runIOAction (Print s) state       = putStr s
runIOAction (ReadLine) state      = getLine >> return ()
runIOAction (ReadFile _) state    = putStrLn "The IOAction ReadFile is useless unless it is used with compIO"
runIOAction (PureIO _) state      = return ()
runIOAction (CallCommand c) state = callCommand c
runIOAction (Composed a f) state  = case a of
    Print s -> runIOAction (Print s) state >> tryRun (FCall (Value $ Eager f) (Value $ Eager NullV))
    CallCommand c -> runIOAction (CallCommand c) state >> tryRun (FCall (Value $ Eager f) (Value $ Eager NullV))
    ReadLine -> readBasic (strAsRTV <$> getLine)
    ReadFile fp -> readBasic (strAsRTV <$> readFile fp)
    PureIO v -> readBasic (return v)
    where readBasic io = io >>= (\c -> tryRun $ FCall (Value $ Eager f) (Value $ Eager c))
          tryRun ex = let (x, state') = runState (eval ex) state in case x of
                       IOV a -> runIOAction a state'
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
                              isImport (Import _ _ _) = True
                              isImport _ = False
                              qF (Def (NormalDef did dex)) = if isQualified then Def $ NormalDef (getImportFromPath path did) dex else Def $ NormalDef did dex
                              --TODO: qF (Def (DestDef))
                              sts' = (qF <$> filter isDef sts) ++ filter isDefDest sts ++ filter isDefFC sts ++ filter isImport sts

getImportFromPath :: String -> String -> String
getImportFromPath path did = (last $ endBy "/" path) ++ "." ++ did

-- TODO:
evalVar :: EvalVar
evalVar (Lazy e) state = evalfst' e state
evalVar (Eager v) _    = v

dest2Eval :: String -> Expr -> Expr -> RTState -> Expr
dest2Eval v e ne state = (Let (NormalDef v e) ne)

-- \l -> let l = l in head l
-- Let (NormalDef l (Var l) {head l})

--TODO: TEMP
evalfst' :: Expr -> RTState -> RTValue
evalfst' ex state = fst $ runState (eval ex) state

eval :: Expr -> State RTState RTValue
eval (Value (Eager (NativeF f cls)))  = do
    state <- get
    return $ NativeF f (M.unions [(getArgs state), (getClosures state),cls])
eval (Value val)                      = case val of
    Eager x -> return x
    Lazy e -> eval e
-- TODO:                                                         Update state accordingly
eval (FCall fx ax)                    = get >>= \state -> do
    av <- eval ax
    case av of
        ExceptionV eType eMsg ed st -> return $ ExceptionV eType eMsg ed st
        _ -> do
            let state' = updateStackTrace ((toFunName fx):) state
--TODO:     Maybe unneccessary?
            put state'
            case fst $ runState (eval fx) state' of
                (FuncV pn ex cls)   -> do
                    put (updateClosures (M.union cls) $ updateArgs (const (Eager <$> M.singleton pn av)) state')
                    x <- eval ex
                    put state
                    return x
                (NativeF f cls)     -> return $ f av (updateClosures (M.union cls) state')
                (FClass n xs cls)   -> do
                    evalFC (FClass n xs cls) cls state' av
                ExceptionV et em ed st -> return $ ExceptionV et em ed st
                x                   -> return $ ExceptionV "Type" ("Tried to call a value that is not a function! The value was '" ++ show x ++ "'") NullV (getStackTrace state)
    where
        toFunName (Var n) = n
        toFunName (Literal (LambdaL _ _)) = "[Lambda]"
        toFunName (If _ _ _) = "[If]"
        toFunName (FCall fe _) = "[FCall(" ++ (toFunName fe) ++ ")]"
        toFunName _ = "[Function]"
        evalFC :: RTValue -> [Variable] -> RTState -> RTValue -> State RTState RTValue
        evalFC (FClass _ [] _) icls istate av = get >>= \state -> return $ ExceptionV "NonExhaustiveFClass" ("FClass cases are not exhaustive!") NullV (getStackTrace state)
        evalFC (FClass 0 ((FClassInstance _ fe):fcs) []) icls istate av = do
            state <- get
            x <- eval (FCall fe (Value $ Eager av))
            case x of
                (ExceptionV "FClass" _ _ _) -> do
--TODO:             Maybe unneccessary?
                    put istate
                    evalFC (FClass 0 fcs icls) icls istate av
                x -> return x

        evalFC (FClass 0 ((FClassInstance p fe):fcs) cls) icls istate av = do
            x <- eval fe
            case x of
                (FuncV pn ex fcls) -> do
                    let (a:as) = cls
                    state <- get
                    put (updateClosures (M.insert pn a) state)
                    x <- evalFC (FClass 0 ((FClassInstance p (FCall fe (Value a))):fcs) as) icls istate av
                    put state
                    return x
                (ExceptionV t m d st) -> return $ ExceptionV t m d st
                x -> error $ "Some other functiontype (" ++ show x ++ ")"
                --(NativeF f fcls) -> do
                --    let (a:as) = cls
                --    state <- get
--TODO:         More than FuncV?
        evalFC (FClass n fcs cls) icls istate av = return (FClass (n - 1) fcs ((Eager av):cls))


eval (Let (NormalDef n vx) ex) = do
        vx' <- eval vx
        state <- get
        put (updateArgs (M.insert n (Eager vx')) state)
        x <- eval ex
        put state
        return x

eval (Let (DestDef dn ns e) ex) = get >>= \state -> case find (\(Destr dn' _ _) -> dn == dn') (getDests state) of
    Nothing -> return $ ExceptionV "State" ("Destructuring " ++ dn ++ " does not exist!") NullV (getStackTrace state)
    Just (Destr _ v exps) -> do
        put (updateArgs (insertAll ((\(n, ne) -> (n, Lazy (dest2Eval v e ne state))) <$> (zip ns exps))) state)
        x <- eval ex
        put state
        return x

eval (Var n) = get >>= \state -> case M.lookup n (getArgs state) <|> M.lookup n (getClosures state) <|> M.lookup n (getVals state) <|> ((\(FClassObj n fis cls) -> Eager $ FClass n fis cls) <$> M.lookup n (getFClasses state)) of
                            Just x  -> case x of
                                Lazy e  -> do
                                            x <- eval e
                                            state' <- get
                                            -- Only single put?
                                            put (updateVal (const $ Eager x) n state')
                                            return x
                                Eager x -> return x
                            Nothing -> return $ ExceptionV "State" ("Value " ++ n ++ " does not exist in the current state! \n\nCurrent Args were: " ++
                                show (getArgs state) ++ "\n\nClosures were: "
                                ++ show (getClosures state) ++ "\n\nVals were:" ++ show (M.keys (getVals state))) NullV (getStackTrace state)

eval (If c th el)               = get >>= \state -> do
    c' <- eval c 
    case (c') of
        BoolV False        -> eval el
        NumV 0             -> eval el
        NullV              -> eval el
        ListV []           -> eval el
        RecordV []         -> eval el
        ExceptionV t m d s -> return $ ExceptionV t m d s
        _                -> eval th

eval (Literal l)                = get >>= \state -> case l of
    NumL x       -> return $ NumV x
    CharL x      -> return $ CharV x
    BoolL x      -> return $ BoolV x
    NullL        -> return NullV
    ListL xps    -> do 
        args <- mapM eval xps
        let ex = find isException args
        case ex of 
            Just e  -> return e
            Nothing -> return $ ListV $ args
    LambdaL n xp -> return $ FuncV n xp (M.union (getArgs state) (getClosures state))
    RecordL xps     -> do
        let ks = (\(x, y) -> x) <$> xps
        vs <- mapM (\(x, y) -> eval y) xps
        let args = zip ks vs
        let ex = find (isException . snd) args
        case ex of
            Just e -> return $ snd e
            Nothing -> return $ RecordV args              

isException :: RTValue -> Bool
isException (ExceptionV _ _ _ _) = True
isException _ = False

evalF :: EvalVar -> RTValue -> RTState -> RTValue
evalF eV x state = case rtVAsMStr x of
    Just s -> case parseEval [] s of
        Left e -> ExceptionV "Parse" (show e) NullV (getStackTrace state)
--TODO:             -------
        Right ex -> evalfst' ex state
    Nothing -> ExceptionV "Type" ("Can only evaluate Strings. '" ++ show x ++ "' is not a String") NullV (getStackTrace state)


showST :: [String] -> String
showST st = if (null st) then "" else foldl1 (\acc -> \cur -> acc ++ " <- " ++ cur) st