module NativeFs where

import Types
import Data.Maybe
import Lib
import Data.List
import qualified Data.Map as M
import Data.Bifunctor
import GHC.IO.FD (stdin)

compIOF :: EvalVar -> RTValue
compIOF eV = FuncV "io" (Value $ Eager $ NativeF compInner M.empty) M.empty
    where
        compInner :: RTValue -> RTState -> RTValue
        compInner f state = case io of
            IOV a -> IOV $ Composed a f
            x -> ExceptionV "Type" ("compIO needs its first argument to be an IO action. '" ++ show x ++ "' is not an IO action!") (getStackTrace state)
            where
                io = fromMaybe (NumV $ -999) $ (`eV` state) <$> M.lookup "io" (getClosures state)

sysF :: EvalVar -> RTValue -> RTState -> RTValue
sysF eV v state = case (rtVAsMStr v) of
    Just s  -> IOV (CallCommand s)
    Nothing -> ExceptionV "Type" "sys needs its argument to be a String!" (getStackTrace state)
    

readLineF :: EvalVar -> RTValue
readLineF eV = IOV ReadLine

--getStdInF :: RTValue
--getStdInF = stdin

throwF :: EvalVar -> RTValue
throwF eV = FuncV "type" (Value $ Eager $ NativeF throwInner M.empty) M.empty
    where
        throwInner :: RTValue -> RTState -> RTValue
        throwInner name state = ExceptionV (rtVAsMStr typen |> fromMaybe "Unknown") (rtVAsMStr name |> fromMaybe "Unknown") (getStackTrace state)
            where
                typen = fromMaybe (NumV $ -999) $ (`eV` state) <$> M.lookup "type" (getClosures state)

remF :: EvalVar -> RTValue
remF eV = FuncV "x" (Value $ Eager $ NativeF remInner M.empty) M.empty
   where
        remInner :: RTValue -> RTState -> RTValue
        remInner y state = case (x, y) of
            (NumV a, NumV b) -> NumV $ fromIntegral (rem (round a) (round b))
            (x, y) -> ExceptionV "Type" ("Rem needs both arguments to be Numbers! '"  ++ show x ++ "' and '" ++ show y ++ "' are not numbers!") (getStackTrace state)
            where
               x = fromMaybe (NumV $ -999) $ (`eV` state) <$> M.lookup "x" (getClosures state)

showNumF :: EvalVar -> RTValue -> RTState -> RTValue
showNumF eV (NumV x) state = strAsRTV $ show x
showNumF eV _        state = ExceptionV "Type" "Not a Number" (getStackTrace state)

putF :: EvalVar -> RTValue -> RTState -> RTValue
putF eV (ListV vs) state = IOV $ Print $ rtVAsStr (ListV vs)
putF eV _ state = ExceptionV "Type" ("'put' only works with strings. Use 'print' if you want to print other types") (getStackTrace state)

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

addNumF :: EvalVar -> RTValue
addNumF eV = FuncV "x" (Value $ Eager $ NativeF addFInner M.empty) M.empty
    where
        addFInner :: RTValue -> RTState -> RTValue
        addFInner y state = case (x, y) of
            (NumV a,  NumV b)   -> (NumV $ a + b)
            (x, y)              -> ExceptionV "Type" ("addNum takes two Nums not '" ++ show x ++ "' and '" ++ show y ++ "'") (getStackTrace state)
            where
                x = fromMaybe (NumV $ -999) $ (`eV` state) <$>  M.lookup "x" (getClosures state)

subNumF :: EvalVar -> RTValue
subNumF eV = FuncV "x" (Value $ Eager $ NativeF subFInner M.empty) M.empty
    where
        subFInner :: RTValue -> RTState -> RTValue
        subFInner y state = case (x, y) of
            (NumV a,  NumV b)   -> (NumV $ a - b)
            (x, y)              -> ExceptionV "Type" ("Cannot subtract the expressions '" ++ show x ++ "' and '" ++ show y ++ "'") (getStackTrace state)
            where
                x = fromMaybe (NumV $ -999) $ (`eV` state) <$> M.lookup "x" (getClosures state)

ordF :: EvalVar -> RTValue
ordF eV = FuncV "x" (Value $ Eager $ NativeF ordFInner M.empty) M.empty
    where
        ordFInner :: RTValue -> RTState -> RTValue
        ordFInner y state = case (x, y) of
            (NumV a, NumV b)     -> ordToRTInt (compare a b)
            (BoolV a, BoolV b)   -> ordToRTInt (compare a b)
            (CharV a, CharV b)   -> ordToRTInt (compare a b)
            (ListV xs, ListV ys) -> ordToRTInt (case compare (length xs) (length ys) of
                            EQ -> if xs == ys then EQ else LT
                            x -> x)
            (RecordV xs, RecordV ys)   -> ordToRTInt (case compare (length xs) (length ys) of
                            EQ -> if xs == ys then EQ else LT
                            x -> x)
            (NullV, NullV)       -> NumV 0
            (NullV, _)           -> NumV $ -1
            (_, NullV)           -> NumV 1
            (x, y)               -> ExceptionV "Type" ("cannot compare the values '" ++ show x ++ "' and '" ++ show y ++
                "' because they are different types and neither of then is 'Null'") (getStackTrace state)
            where
                x = fromMaybe (NumV $ -999) $ (`eV` state) <$> M.lookup "x" (getClosures state)

ordToRTInt :: Ordering -> RTValue
ordToRTInt LT = NumV $ -1
ordToRTInt GT = NumV 1
ordToRTInt EQ = NumV 0

mulNumF :: EvalVar -> RTValue
mulNumF eV = FuncV "x" (Value $ Eager $ NativeF mulFInner M.empty) M.empty
    where
        mulFInner :: RTValue -> RTState -> RTValue
        mulFInner y state = case (x, y) of
            (NumV a,  NumV b)   -> (NumV $ a * b)
            (x, y)              -> ExceptionV "Type" ("Cannot multiply the expressions '" ++ show x ++ "' and '" ++ show y ++ "'") (getStackTrace state)
            where
                x = fromMaybe (NumV $ -999) $ (`eV` state) <$> M.lookup "x" (getClosures state)

divNumF :: EvalVar -> RTValue
divNumF eV = FuncV "x" (Value $ Eager $ NativeF divFInner M.empty) M.empty
    where
        divFInner :: RTValue -> RTState -> RTValue
        divFInner y state = case (x, y) of
            (NumV a,  NumV b)   -> (NumV $ a / b)
            (x, y)              -> ExceptionV "Type" ("Cannot divide the expressions '" ++ show x ++ "' and '" ++ show y ++ "'") (getStackTrace state)
            where
                x = fromMaybe (NumV $ -999) $ (`eV` state) <$> M.lookup "x" (getClosures state)



debugRawF :: EvalVar -> RTValue -> RTState -> RTValue
debugRawF eV x state = strAsRTV $ show x


debugStateF :: EvalVar -> RTValue -> RTState -> RTValue
debugStateF eV _ state = strAsRTV (show state)

headF :: EvalVar -> RTValue -> RTState -> RTValue
headF eV x state = case x of
    ListV []    -> NullV
    ListV (x:_) -> x
    x           -> ExceptionV "Type" ("'head' needs its argument to be a list. '" ++ show x ++ "' is not a List!") (getStackTrace state)

tailF :: EvalVar -> RTValue -> RTState -> RTValue
tailF eV x state = case x of
    ListV []     -> NullV
    ListV (_:xs) -> ListV xs
    x            -> ExceptionV "Type" ("'tail' needs its argument to be a list. '" ++ show x ++ "' is not a List!") (getStackTrace state)

typeofF :: EvalVar -> RTValue -> RTState -> RTValue
typeofF eV (IOV _)            state = strAsRTV "IO"
typeofF eV (CharV _)          state = strAsRTV "Char"
typeofF eV (ListV _)          state = strAsRTV "List"
typeofF eV (NumV _)           state = strAsRTV "Num"
typeofF eV (BoolV _)          state = strAsRTV "Bool"
typeofF eV (FuncV _ _ _)      state = strAsRTV "Function"
typeofF eV (NativeF _ _)    state = strAsRTV "Function"
typeofF eV (FClass _ _ _)     state = strAsRTV "Function"
typeofF eV (NullV)            state = strAsRTV "Null"
typeofF eV (RecordV _)        state = strAsRTV "Record"
typeofF eV (ExceptionV _ _ _) state = strAsRTV "Exception"

consF :: EvalVar -> RTValue
consF eV = FuncV "x" (Value $ Eager $ NativeF consInner M.empty) M.empty
    where
        consInner :: RTValue -> RTState -> RTValue
        consInner xs state = case xs of
            ListV l -> ListV (x:l)
            x       -> ExceptionV "Type" ("cons needs its second argument to be of type List. The value '" ++ show x ++ "' is not a List!") (getStackTrace state)
            where
                x = fromMaybe (NumV $ -999) $ (`eV` state) <$> M.lookup "x" (getClosures state)


getF :: EvalVar -> RTValue
getF eV = FuncV "n" (Value $ Eager $ NativeF getInner M.empty) M.empty
    where
        getInner :: RTValue -> RTState -> RTValue
        getInner ms state = case ms of
            RecordV m -> case rtVAsMStr n of
                Nothing -> ExceptionV "Type" ("get needs its first argument to be of type String. The value '" ++ show n ++ "' is not a String!") (getStackTrace state)
                Just s  -> fromMaybe NullV $ lookup s m
            x -> ExceptionV "Type" ("get needs its second argument to be of type Map. The value '" ++ show x ++ "' is not a Map!") (getStackTrace state)
            where
                n = fromMaybe (NumV $ -999) $ (`eV` state) <$> M.lookup "n" (getClosures state)

setF :: EvalVar -> RTValue
setF eV = FuncV "n" (Literal (LambdaL "x" (Value $ Eager $ NativeF setInner M.empty))) M.empty
    where
        setInner :: RTValue -> RTState -> RTValue
        setInner ms state = case ms of
            RecordV m -> case rtVAsMStr n of
                Nothing -> ExceptionV "Type" ("set needs its first argument to be of type String. The value '" ++ show n ++ "' is not a String!") (getStackTrace state)
                Just s  -> RecordV $ setAL s x m
            x -> ExceptionV "Type" ("set needs its second argument to be of type Map. The value '" ++ show x ++ "' is not a Map!") (getStackTrace state)
            where
                n = fromMaybe (NumV $ -999) $ (`eV` state) <$> M.lookup "n" (getClosures state)
                x = fromMaybe (NumV $ -777) $ (`eV` state) <$> M.lookup "x" (getClosures state)

pureIOF :: EvalVar -> RTValue -> RTState -> RTValue
pureIOF eV x state = IOV $ PureIO x


roundF :: EvalVar -> RTValue -> RTState -> RTValue
roundF eV (NumV n) state = NumV $ fromIntegral $ round n
roundF eV x state        = ExceptionV "Type" ("round needs its first argument to be of type Number. The value '" ++ show x ++ "' is not a Number.") (getStackTrace state)


entriesF :: EvalVar -> RTValue -> RTState -> RTValue
entriesF eV (RecordV es) state = ListV ((\(x, y) -> ListV [strAsRTV x, y]) <$> es)
entriesF eV _ state            = ExceptionV "Type" ("Not a Record") (getStackTrace state)

toCodeF :: EvalVar -> RTValue -> RTState -> RTValue
toCodeF eV v state = strAsRTV "NYI"--(toCode v)

--toCode :: RTValue -> String
--toCode (NumV x)           = show x
--toCode (CharV x)          = show x
--toCode (BoolV b)          = show b
--toCode (NullV)            = "Null"
--toCode (ListV xs)         = show (toCode <$> xs)
--toCode (IOV _)            = "<IO>"
--toCode (ExceptionV t m _) = t ++ " Exception: '" ++ m ++ "'"
--toCode (RecordV es)       = show (map (second toCode) es)
--toCode (NativeF _ _)      = "<NativeF>"
--toCode (FuncV p e _)      = "\\" ++ p ++ " -> " ++ toCodeE e
--toCode x                  = "UNKNOWN(" ++ show x ++ ")"
--
--toCodeE :: Expr -> String
--toCodeE (Value v) = "VAL(" ++ toCode v ++ ")"
--toCodeE (Var n) = n
--toCodeE (FCall fe ae) = "(" ++ (toCodeE fe) ++ " " ++ toCodeE ae ++ ")"
--toCodeE (If ie te ee) = "if " ++ (toCodeE ie) ++ " then " ++ toCodeE te ++ " else " ++ toCodeE ee ++ ""
--toCodeE (Let (NormalDef n de) e) = "let " ++ n ++ " = " ++ toCodeE de ++ " in " ++ toCodeE e ++ ""
--toCodeE (Let (DestDef dn ds de) e) = "let " ++ dn ++ " " ++ (foldl1 (\a -> \c -> a ++ " " ++ c) ds) ++ " = " ++ toCodeE de ++ " in " ++ toCodeE e ++ ""
--toCodeE (Literal (NumL n)) = show n
--toCodeE (Literal (CharL c)) = show c
--toCodeE (Literal (BoolL b)) = show b
--toCodeE (Literal NullL) = "Null"
--toCodeE (Literal (ListL xs))
--    | all isChar xs = show (mapM toCodeE xs)
--    | otherwise     = show (toCodeE <$> xs)
--    where isChar (Literal (CharL _)) = True
--          isChar _ = False
--toCodeE (Literal (RecordL es)) = show (map (second toCodeE) es)
--toCodeE (Literal (LambdaL p e)) = "\\" ++ p ++ " -> " ++ toCodeE e

