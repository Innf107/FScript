{-# LANGUAGE FlexibleInstances #-}
module Types where

import Data.Map as M


data Statement = NOP
               | Import String ImportType Bool
               | Def Definition
               | DefDest Destr
-- TODO DefFCArity String Int
               | DefFClass String Int FClassInstance
               | Run Expr
               deriving (Show, Eq, Read)

data Definition = NormalDef String Expr
                | DestDef String [String] Expr
                deriving (Show, Eq, Read)

data FClassInstance = FClassInstance Int Expr deriving (Show, Eq, Read)

-- TODO: As
data ImportType = All
                | Exposing [String]
    deriving (Show, Eq, Read)

data Expr = Literal Lit
          | Let Definition Expr
          | If Expr Expr Expr
          | Var String
          | FCall Expr Expr
          | Value RTValue
          deriving (Show, Eq, Read)

data Lit = NumL Double
         | CharL Char
         | ListL [Expr]
         | BoolL Bool
         | NullL
         | LambdaL String Expr
         | RecordL [(String, Expr)]
         deriving (Show, Eq, Read)

data RTValue = NumV Double
             | CharV Char
             | ListV [RTValue]
             | BoolV Bool
             | IOV IOAction
             | FuncV String Expr (M.Map String RTValue)
             | NativeF (RTValue -> RTState -> RTValue) (M.Map String RTValue)
             | FClass Int [FClassInstance] [RTValue]
             | NullV
             | RecordV [(String, RTValue)]
             | ExceptionV String String [String]
             deriving (Show, Eq, Read)


data RTState = RTState {getVals::M.Map String RTValue, getArgs::M.Map String RTValue,
                        getClosures::M.Map String RTValue, getDests::[Destr],
                        getFClasses::M.Map String FClassObj,
                        getStackTrace::[String]}
                        deriving (Show, Eq, Read)

data FClassObj = FClassObj Int [FClassInstance] [RTValue] deriving (Show, Eq, Read)

data Destr = Destr String String [Expr] deriving (Show, Eq, Read)

data IOAction = PureIO RTValue
              | Print String
              | ReadLine
              | ReadFile String
              | Composed IOAction RTValue
              deriving (Show, Eq, Read)


instance Show (RTValue -> RTState -> RTValue) where
    show _ = "(RTValue -> RTState -> RTValue)"
instance Eq (RTValue -> RTState -> RTValue) where
    _ == _ = False

instance Read (RTValue -> RTState -> RTValue) where
    readsPrec _ _ = []
