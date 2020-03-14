{-# LANGUAGE FlexibleInstances #-}
module Types where

import Data.Map as M
import Text.Parsec.Expr (Assoc(..))

data Op = Op String Integer Assoc deriving (Show, Read, Eq)

data Statement = NOP
               | DefOpPrec String Integer Assoc
               | Import String ImportType Bool
               | Def Definition
               | DefDest Destr
-- TODO DefFCArity String Int
               | DefFClass String Int FClassInstance
               | Run Expr
               deriving (Show, Eq, Read)

data Definition = NormalDef String Expr
--                Let     (HT     x xs )= [1,2]
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
          | Value Variable
          deriving (Show, Eq, Read)

data Variable = Lazy Expr | Eager RTValue deriving (Show, Eq, Read)

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
             | FuncV String Expr (M.Map String Variable)
             | NativeF (RTValue -> RTState -> RTValue) (M.Map String Variable)
             | FClass Int [FClassInstance] [Variable]
             | NullV
             | RecordV [(String, RTValue)]
             | ExceptionV String String RTValue [String]
             deriving (Show, Eq, Read)

type EvalVar = (Variable -> RTState -> RTValue)


data RTState = RTState {getVals::M.Map String Variable, getArgs::M.Map String Variable,
                        getClosures::M.Map String Variable, getDests::[Destr],
                        getFClasses::M.Map String FClassObj,
                        getStackTrace::[String],
                        getEvaledVals::M.Map String RTValue, getEvaledArgs::M.Map String RTValue,
                        getEvaledCls::M.Map String RTValue}
                        deriving (Show, Eq, Read)

data FClassObj = FClassObj Int [FClassInstance] [Variable] deriving (Show, Eq, Read)

--                 HT     l     [head l, tail l]
data Destr = Destr String String [Expr] deriving (Show, Eq, Read)

data IOAction = PureIO RTValue
              | Print String
              | ReadLine
              | ReadFile String
              | CallCommand String
              | Composed IOAction RTValue
              deriving (Show, Eq, Read)


instance Show (RTValue -> RTState -> RTValue) where
    show _ = "(RTValue -> RTState -> RTValue)"
instance Show (Variable -> RTState -> RTValue) where
    show _ = "(Variable -> RTState -> RTValue)"

instance Eq (RTValue -> RTState -> RTValue) where
    _ == _ = False
instance Eq (Variable -> RTState -> RTValue) where
        _ == _ = False
instance Read (RTValue -> RTState -> RTValue) where
    readsPrec _ _ = []
instance Read (Variable -> RTState -> RTValue) where
    readsPrec _ _ = []


instance Read Assoc where
    readsPrec _ _ = []

instance Eq Assoc where
    AssocNone == AssocNone = True
    AssocLeft == AssocLeft = True
    AssocRight == AssocRight = True
    _ == _ = False

instance Show Assoc where
    show AssocNone = "AssocNone"
    show AssocLeft = "AssocLeft"
    show AssocRight= "AssocRight"

strAsRTV :: String -> RTValue
strAsRTV s = ListV $ fmap CharV s

strAsEx :: String -> Expr
strAsEx = Literal . ListL . fmap (Literal . CharL)
