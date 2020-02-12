module NativeFs where

import Parser
import Data.Maybe
import Lib
import Data.List

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

remF :: RTValue
remF = FuncV "x" (Value $ NativeF remInner []) []
   where
        remInner :: RTValue -> RTState -> RTValue
        remInner y state = case (x, y) of
            (NumV a, NumV b) -> NumV $ fromIntegral (rem (round a) (round b))
            (x, y) -> Exception "Type" $ "Rem needs both arguments to be Numbers! '"  ++ show x ++ "' and '" ++ show y ++ "' are not numbers!"
            where
               x = fromMaybe (NumV $ -999) $ lookup "x" (getClosures state)
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
            (MapV xs, MapV ys)   -> ordToRTInt (case compare (length xs) (length ys) of
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

divF :: RTValue
divF = FuncV "x" (Value $ NativeF divFInner []) []
    where
        divFInner :: RTValue -> RTState -> RTValue
        divFInner y state = case (x, y) of
            (NumV a,  NumV b)   -> (NumV $ a / b)
            --(ListV a, ListV b)  -> (ListV $ unique a b)
            --(BoolV a, BoolV b)  -> (BoolV $ if b then False else a)
            --(NullV, x)          -> NullV
            --(x, NullV)          -> x
            (x, y)              -> Exception "Type" $ "Cannot divide the expressions '" ++ show x ++ "' and '" ++ show y ++ "'"
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


