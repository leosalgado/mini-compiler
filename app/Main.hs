module Main where

import AST

main :: IO ()
main = do
  let expr = Add (Sub (Number 10) (Number 5)) (Var "y")
  print expr
