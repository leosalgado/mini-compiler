module Main where

import Lexer
import Parser
import System.IO

main :: IO ()
main = do
  putStrLn "Mini Compiler REPL"
  putStrLn "Statement:"
  repl

repl :: IO ()
repl = do
  putStr "λ "
  hFlush stdout
  input <- getLine
  if input == ":quit"
    then putStrLn "Bye!"
    else do
      let tokens = tokenize input
      print $ parseFullStmt tokens
      repl
