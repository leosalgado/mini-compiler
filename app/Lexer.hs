module Lexer where

import AST (Number (FloatNum, IntNum))
import Data.Char (isAlpha, isDigit)

data Token
  = TNumber Number
  | TIdent String
  | TPlus
  | TMinus
  | TMul
  | TDiv
  | TPow
  | TAssign
  | TLParen
  | TRParen
  | TComma
  | TFunc
  | TEq
  | TEOF
  deriving (Show, Eq)

lexNumber :: [Char] -> [Token]
lexNumber cs =
  let (numStr, rest) = span (\x -> isDigit x || x == '.') cs
      token =
        if '.' `elem` numStr
          then TNumber (FloatNum (read numStr))
          else TNumber (IntNum (read numStr))
   in token : lexer rest

lexIdent :: [Char] -> [Token]
lexIdent cs =
  let (idStr, rest) = span isAlpha cs
   in TIdent idStr : lexer rest

lexer :: String -> [Token]
lexer [] = [TEOF]
lexer (c : cs)
  | c == '+' = TPlus : lexer cs
  | c == '-' = TMinus : lexer cs
  | c == '*' = TMul : lexer cs
  | c == '/' = TDiv : lexer cs
  | c == '^' = TPow : lexer cs
  | c == '(' = TLParen : lexer cs
  | c == ')' = TRParen : lexer cs
  | c == ',' = TComma : lexer cs
  | c == '=' = TAssign : lexer cs
  | c `elem` [' ', '\n', '\t'] = lexer cs
  | isDigit c = lexNumber (c : cs)
  | isAlpha c = lexIdent (c : cs)
  | otherwise = error ("Unexpected character: " ++ [c])

-- Acces via alias "tokenize"
tokenize :: String -> [Token]
tokenize = lexer
