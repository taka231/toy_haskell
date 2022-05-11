module AST where

import           Data.Text (Text)

data Expr
    = Num Integer
    | Bool Bool
    | BiOp Expr Text Expr
    | If Expr Expr Expr
    | Var Text
    | Assign Text Expr
    | Unit
    deriving (Show, Eq)

data Value
    = VNum Integer
    | VBool Bool
    | VUnit
    deriving (Show, Eq)

data Types
    = TNum
    | TBool
