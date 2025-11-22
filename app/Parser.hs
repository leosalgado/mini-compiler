module Parser where

import AST
import Lexer

-- complete parsing
parseFullStmt :: [Token] -> Stmt
parseFullStmt tokens =
  let (stmt, rest) = parseStmt tokens
   in case rest of
        [TEOF] -> stmt
        _ -> error $ "Unexpected tokens at the end: " ++ show rest

-- statements
parseStmt :: [Token] -> (Stmt, [Token])
parseStmt (TIdent "func" : TIdent name : TLParen : rest) =
  let (params, rest') = parseParams rest
   in case rest' of
        (TAssign : rest'') ->
          let (body, rest''') = parseExpr rest''
           in (FunDef name params body, rest''')
        _ -> error $ "Expected '=' after function parameters, got: " ++ show rest'
parseStmt (TIdent var : TAssign : rest) =
  case rest of
    [] -> error $ "Assignment to '" ++ var ++ "' is incomplete - expected expression after '='"
    _ ->
      let (expr, rest') = parseExpr rest
       in (Assign var expr, rest')
parseStmt tokens =
  let (expr, rest) = parseExpr tokens
   in (ExprStmt expr, rest)

-- funcs
parseParams :: [Token] -> ([String], [Token])
parseParams (TIdent v : TComma : rest) =
  let (vs, rest') = parseParams rest
   in (v : vs, rest')
parseParams (TIdent v : TRParen : rest) = ([v], rest)
parseParams (TIdent v : TIdent v2 : _) =
  error $ "Missing comma between parameters '" ++ v ++ "' and '" ++ v2 ++ "'"
parseParams (TRParen : rest) = ([], rest)
parseParams (TNumber _ : _) = error "Invalid parameter: expected variable name, got number"
parseParams (TComma : _) = error "Invalid parameter: unexpected ',' - expected variable name"
parseParams [] = error "Unexpected end of input in function parameters - expected ')'"
parseParams (t : _) = error $ "Invalid function parameter: expected variable name or ')', got " ++ show t

-- =======
-- ops
-- =======
parseExpr :: [Token] -> (Expr, [Token])
parseExpr = parseAdd

-- plus & minus
parseAdd :: [Token] -> (Expr, [Token])
parseAdd tokens =
  let (lhs, rest) = parseMul tokens
   in case rest of
        (TPlus : rest') ->
          let (rhs, rest'') = parseAdd rest'
           in (Add lhs rhs, rest'')
        (TMinus : rest') ->
          let (rhs, rest'') = parseAdd rest'
           in (Sub lhs rhs, rest'')
        _ -> (lhs, rest)

-- mul & div
parseMul :: [Token] -> (Expr, [Token])
parseMul tokens =
  let (lhs, rest) = parsePow tokens
   in case rest of
        (TMul : rest') ->
          let (rhs, rest'') = parseMul rest'
           in (Mul lhs rhs, rest'')
        (TDiv : rest') ->
          let (rhs, rest'') = parseMul rest'
           in (Div lhs rhs, rest'')
        _ -> (lhs, rest)

-- pow
parsePow :: [Token] -> (Expr, [Token])
parsePow tokens =
  let (lhs, rest) = parseAtom tokens
   in case rest of
        (TPow : rest') ->
          let (rhs, rest'') = parsePow rest'
           in (Pow lhs rhs, rest'')
        _ -> (lhs, rest)

-- rest
parseAtom :: [Token] -> (Expr, [Token])
parseAtom (TNumber n : rest) = (Number n, rest)
parseAtom (TIdent v : rest) = (Var v, rest)
parseAtom (TLParen : rest) =
  let (e, rest') = parseExpr rest
   in case rest' of
        (TRParen : rest'') -> (e, rest'')
        _ -> error $ "Expected ) to close parenthesis, but got: " ++ show rest'
parseAtom (TRParen : _) = error "Unexpected ')' - missing opening parenthesis or operand"
parseAtom (TPlus : _) = error "Unexpected '+' operator - expected number, variable, or '('"
parseAtom (TMinus : _) = error "Unexpected '-' operator - expected number, variable, or '('"
parseAtom (TMul : _) = error "Unexpected '*' operator - expected number, variable, or '('"
parseAtom (TDiv : _) = error "Unexpected '/' operator - expected number, variable, or '('"
parseAtom (TPow : _) = error "Unexpected '^' operator - expected number, variable, or '('"
parseAtom (TAssign : _) = error "Unexpected '=' - cannot use assignment inside expression"
parseAtom (TComma : _) = error "Unexpected ',' - commas only allowed in function parameters"
parseAtom (TEOF : _) = error "Unexpected end of input - expected number, variable, or '('"
parseAtom [] = error "Unexpected end of input - expected number, variable, or '('"
parseAtom (t : _) = error $ "Unexpected token: " ++ show t ++ " - expected number, variable, or '('"
