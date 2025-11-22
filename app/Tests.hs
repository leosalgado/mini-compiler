module Main where

import Control.Exception (SomeException, catch)
import Lexer
import Parser

testValid :: [String]
testValid =
  [ "3 + 4 * 5",
    "(3 + 4) * 5",
    "2^3^4",
    "x = 10",
    "func sum(a, b) = a + b",
    "func constant() = 42",
    "10 - 5 + 3"
  ]

testInvalid :: [String]
testInvalid =
  [ "3 + + 4",
    "3 +",
    "(3 + 4",
    "3 + 4)",
    "* 5",
    "func f(x y) = x",
    "x =",
    "func f() ="
  ]

main :: IO ()
main = do
  putStrLn "=== VALID TESTS ==="
  mapM_ testOne testValid

  putStrLn "\n=== INVALID TESTS (should fail) ==="
  mapM_ testOneError testInvalid

testOne :: [Char] -> IO ()
testOne input = do
  putStrLn $ "\nInput: " ++ input
  let tokens = tokenize input
  print $ parseFullStmt tokens

testOneError :: [Char] -> IO ()
testOneError input = do
  putStrLn $ "\nInput: " ++ input
  let tokens = tokenize input
  catch
    (print $ parseFullStmt tokens)
    (\e -> putStrLn $ "  ✓ Error detected: " ++ show (e :: SomeException))
  return ()
