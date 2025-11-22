module AST where

data Number
  = IntNum Int
  | FloatNum Double
  deriving (Show, Eq)

data Expr
  = Number Number
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Call String [Expr]
  deriving (Show, Eq)

data Stmt
  = Assign String Expr
  | FunDef String [String] Expr
  | ExprStmt Expr
  deriving (Show, Eq)
