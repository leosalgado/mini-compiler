module AST where

data Expr
  = Number Double
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
  deriving (Show, Eq)
