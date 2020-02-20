module NativeFs where

import Parser
import Data.Maybe
import Lib
import Data.List
import qualified Data.Map as M

compIOF :: RTValue
compIOF = FuncV "io" (Value $ NativeF compInner M.empty) M.empty
    where
        compInner :: RTValue -> RTState -> RTValue
        compInner f state = case io of
            IOV a -> IOV $ Composed a f
            x -> ExceptionV "Type" $ "compIO needs its first argument to be an IO action. '" ++ show x ++ "' is not an IO action!"
            where
                io = fromMaybe (NumV $ -999) $ M.lookup "io" (getClosures state)

readLineF :: RTValue
readLineF = IOV ReadLine

throwF :: RTValue
throwF = FuncV "type" (Value $ NativeF throwInner M.empty) M.empty
    where
        throwInner :: RTValue -> RTState -> RTValue
        throwInner name state = ExceptionV (rtVAsMStr typen |> fromMaybe "Unknown") (rtVAsMStr name |> fromMaybe "Unknown")
            where
                typen = fromMaybe (NumV $ -999) $ M.lookup "type" (getClosures state)

remF :: RTValue
remF = FuncV "x" (Value $ NativeF remInner M.empty) M.empty
   where
        remInner :: RTValue -> RTState -> RTValue
        remInner y state = case (x, y) of
            (NumV a, NumV b) -> NumV $ fromIntegral (rem (round a) (round b))
            (x, y) -> ExceptionV "Type" $ "Rem needs both arguments to be Numbers! '"  ++ show x ++ "' and '" ++ show y ++ "' are not numbers!"
            where
               x = fromMaybe (NumV $ -999) $ M.lookup "x" (getClosures state)

showNumF (NumV x) state = strAsRTV $ show x
showNumF _        state = ExceptionV "Type" "Not a Number"

putF :: RTValue -> RTState -> RTValue
putF (ListV vs) state = IOV $ Print $ rtVAsStr (ListV vs)
putF _ state = ExceptionV "Type" $ "'put' only works with strings. Use 'print' if you want to print other types"

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
addF = FuncV "x" (Value $ NativeF addFInner M.empty) M.empty
    where
        addFInner :: RTValue -> RTState -> RTValue
        addFInner y state = case (x, y) of
            (NumV a,  NumV b)   -> (NumV $ a + b)
            (ListV a, ListV b)  -> (ListV $ a ++ b)
            (BoolV a, BoolV b)  -> (BoolV $ a || b)
            (NullV, x)          -> x
            (x, NullV)          -> x
            (x, y)              -> ExceptionV "Type" $ "Cannot add the expressions '" ++ show x ++ "' and '" ++ show y ++ "'"
            where
                x = fromMaybe (NumV $ -999) $ M.lookup "x" (getClosures state)

subF :: RTValue
subF = FuncV "x" (Value $ NativeF subFInner M.empty) M.empty
    where
        subFInner :: RTValue -> RTState -> RTValue
        subFInner y state = case (x, y) of
            (NumV a,  NumV b)   -> (NumV $ a - b)
            (ListV a, ListV b)  -> (ListV $ unique a b)
            (BoolV a, BoolV b)  -> (BoolV $ if b then False else a)
            (x, NullV)          -> x
            (x, y)              -> ExceptionV "Type" $ "Cannot subtract the expressions '" ++ show x ++ "' and '" ++ show y ++ "'"
            where
                x = fromMaybe (NumV $ -999) $ M.lookup "x" (getClosures state)

ordF :: RTValue
ordF = FuncV "x" (Value $ NativeF ordFInner M.empty) M.empty
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
            (x, y)               -> ExceptionV "Type" $ "cannot compare the values '" ++ show x ++ "' and '" ++ show y ++
                "' because they are different types and neither of then is 'Null'"
            where
                x = fromMaybe (NumV $ -999) $ M.lookup "x" (getClosures state)

ordToRTInt :: Ordering -> RTValue
ordToRTInt LT = NumV $ -1
ordToRTInt GT = NumV 1
ordToRTInt EQ = NumV 0

mulF :: RTValue
mulF = FuncV "x" (Value $ NativeF mulFInner M.empty) M.empty
    where
        mulFInner :: RTValue -> RTState -> RTValue
        mulFInner y state = case (x, y) of
            (NumV a,  NumV b)   -> (NumV $ a * b)
            --(ListV a, ListV b)  -> (ListV $ unique a b)
            --(BoolV a, BoolV b)  -> (BoolV $ if b then False else a)
            --(NullV, x)          -> NullV
            --(x, NullV)          -> x
            (x, y)              -> ExceptionV "Type" $ "Cannot multiply the expressions '" ++ show x ++ "' and '" ++ show y ++ "'"
            where
                x = fromMaybe (NumV $ -999) $ M.lookup "x" (getClosures state)

divF :: RTValue
divF = FuncV "x" (Value $ NativeF divFInner M.empty) M.empty
    where
        divFInner :: RTValue -> RTState -> RTValue
        divFInner y state = case (x, y) of
            (NumV a,  NumV b)   -> (NumV $ a / b)
            --(ListV a, ListV b)  -> (ListV $ unique a b)
            --(BoolV a, BoolV b)  -> (BoolV $ if b then False else a)
            --(NullV, x)          -> NullV
            --(x, NullV)          -> x
            (x, y)              -> ExceptionV "Type" $ "Cannot divide the expressions '" ++ show x ++ "' and '" ++ show y ++ "'"
            where
                x = fromMaybe (NumV $ -999) $ M.lookup "x" (getClosures state)



debugRawF :: RTValue -> RTState -> RTValue
debugRawF x state = strAsRTV $ show x

headF :: RTValue -> RTState -> RTValue
headF x state = case x of
    ListV []    -> NullV
    ListV (x:_) -> x
    x           -> ExceptionV "Type" $ "'head' needs its argument to be a list. '" ++ show x ++ "' is not a List!"

tailF :: RTValue -> RTState -> RTValue
tailF x state = case x of
    ListV []     -> NullV
    ListV (_:xs) -> ListV xs
    x            -> ExceptionV "Type" $ "'tail' needs its argument to be a list. '" ++ show x ++ "' is not a List!"

execF :: RTValue -> RTState -> RTValue
execF (IOV a) state = IOV a
execF x       state = ExceptionV "Type" $ "Can only run 'exec' on values of type IO. '" ++ show x ++ "' does not have the type IO!"

typeofF :: RTValue -> RTState -> RTValue
typeofF (IOV _)          state = strAsRTV "IO"
typeofF (CharV _)        state = strAsRTV "Char"
typeofF (ListV _)        state = strAsRTV "List"
typeofF (NumV _)         state = strAsRTV "Num"
typeofF (BoolV _)        state = strAsRTV "Bool"
typeofF (FuncV _ _ _)    state = strAsRTV "Function"
typeofF (NativeF _ _)    state = strAsRTV "Function"
typeofF (FClass _)       state = strAsRTV "Function"
typeofF (NullV)          state = strAsRTV "Null"
typeofF (RecordV _)      state = strAsRTV "Record"
typeofF (ExceptionV _ _) state = strAsRTV "Exception"

consF :: RTValue
consF = FuncV "x" (Value $ NativeF consInner M.empty) M.empty
    where
        consInner :: RTValue -> RTState -> RTValue
        consInner xs state = case xs of
            ListV l -> ListV (x:l)
            x       -> ExceptionV "Type" $ "cons needs its second argument to be of type List. The value '" ++ show x ++ "' is not a List!"
            where
                x = fromMaybe (NumV $ -999) $ M.lookup "x" (getClosures state)


getF :: RTValue
getF = FuncV "n" (Value $ NativeF getInner M.empty) M.empty
    where
        getInner :: RTValue -> RTState -> RTValue
        getInner ms state = case ms of
            RecordV m -> case rtVAsMStr n of
                Nothing -> ExceptionV "Type" $ "get needs its first argument to be of type String. The value '" ++ show n ++ "' is not a String!"
                Just s  -> fromMaybe NullV $ lookup s m
            x -> ExceptionV "Type" $ "get needs its second argument to be of type Map. The value '" ++ show x ++ "' is not a Map!"
            where
                n = fromMaybe (NumV $ -999) $ M.lookup "n" (getClosures state)

setF :: RTValue
setF = FuncV "n" (Literal (LambdaL "x" (Value $ NativeF setInner M.empty))) M.empty
    where
        setInner :: RTValue -> RTState -> RTValue
        setInner ms state = case ms of
            RecordV m -> case rtVAsMStr n of
                Nothing -> ExceptionV "Type" $ "set needs its first argument to be of type String. The value '" ++ show n ++ "' is not a String!"
                Just s  -> RecordV $ setAL s x m
            x -> ExceptionV "Type" $ "set needs its second argument to be of type Map. The value '" ++ show x ++ "' is not a Map!"
            where
                n = fromMaybe (NumV $ -999) $ M.lookup "n" (getClosures state)
                x = fromMaybe (NumV $ -777) $ M.lookup "x" (getClosures state)

pureIOF :: RTValue -> RTState -> RTValue
pureIOF x state = IOV $ PureIO x


roundF :: RTValue -> RTState -> RTValue
roundF (NumV n) state = NumV $ fromIntegral $ round n
roundF x _            = ExceptionV "Type" $ "round needs its first argument to be of type Number. The value '" ++ show x ++ "' is not a Number."


entriesF :: RTValue -> RTState -> RTValue
entriesF (RecordV es) state = ListV ((\(x, y) -> ListV [strAsRTV x, y]) <$> es)
entriesF _ state            = ExceptionV "Type" "Not a Record"
