{-# LANGUAGE OverloadedStrings #-}
module Parser where

import AST
import Data.Void
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import qualified Text.Megaparsec.Char.Lexer as L

-- | The parser type.
type Parser = Parsec Void String

------------------------------------------------------------------------------
-- Whitespace and Lexeme Helpers
------------------------------------------------------------------------------

-- Internal whitespace: only spaces and tabs.
sc :: Parser ()
sc = skipMany (oneOf (" \t" :: String))

-- Layout whitespace: spaces, tabs, and newlines.
scn :: Parser ()
scn = skipMany (oneOf (" \t\n" :: String))

-- Lexeme using internal whitespace.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Symbols using internal or layout whitespace.
symbol, symboln :: String -> Parser String
symbol  = L.symbol sc
symboln = L.symbol scn

-- Parenthesized parser.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

------------------------------------------------------------------------------
-- Identifiers and Reserved Words
------------------------------------------------------------------------------

reservedWords :: [String]
reservedWords =
  [ "let", "in", "if", "then", "else", "fun", "rec"
  , "match", "with", "true", "false", "int", "bool", "and"
  ]

identifier :: Parser String
identifier = lexeme (try $ do
  x <- (:) <$> (letterChar <|> char '_')
           <*> many (alphaNumChar <|> oneOf ("_'" :: String))
  if x `elem` reservedWords
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x)

------------------------------------------------------------------------------
-- Constants and Types
------------------------------------------------------------------------------

parseConstant :: Parser Constant
parseConstant = lexeme $ choice
  [ IntConst <$> L.decimal
  , BoolConst True  <$ symbol "true"
  , BoolConst False <$ symbol "false"
  ]

parseType :: Parser TypeExpr
parseType = makeFunctionType
  where
    parseTypeAtom :: Parser TypeExpr
    parseTypeAtom = choice
      [ symbol "int"  >> return TypeInt
      , symbol "bool" >> return TypeBool
      , TypeVar <$> identifier
      , parens parseType
      ]
    makeFunctionType = do
      t <- parseTypeAtom
      (symbol "->" *> (TypeFunc t <$> parseType)) <|> return t

------------------------------------------------------------------------------
-- Patterns
------------------------------------------------------------------------------

parsePattern :: Parser Pattern
parsePattern = do
  base <- choice
    [ Wildcard <$ symbol "_"
    , try (ConstPattern <$> parseConstant)
    , VarPattern <$> identifier
    , parens parsePattern
    ]
  -- Optionally attach a type annotation to the pattern.
  (symbol ":" *> (TypePattern base <$> parseType)) <|> return base

------------------------------------------------------------------------------
-- Expressions: Constants, Identifiers, and Lambda/Let/If/Match
------------------------------------------------------------------------------

parseConstantExpr :: Parser Expr
parseConstantExpr = ConstantExpr <$> parseConstant

parseIdentifierExpr :: Parser Expr
parseIdentifierExpr = Identifier <$> identifier

parseLambdaExpr :: Parser Expr
parseLambdaExpr = do
  _    <- symbol "fun"
  pats <- some parsePattern
  _    <- symbol "->"
  scn
  body <- parseExpr
  return (Lambda pats body)

parseBinding :: Parser (Pattern, Expr)
parseBinding = do
  pat  <- parsePattern
  _    <- symboln "="
  expr <- parseExpr
  return (pat, expr)

parseLetExpr :: Parser Expr
parseLetExpr = do
  _       <- symbol "let"
  recFlag <- optional (symbol "rec")
  firstB  <- parseBinding
  restBs  <- many (symboln "and" *> parseBinding)
  _       <- symboln "in"
  scn
  body    <- parseExpr
  let allBindings = firstB : restBs
  return $ case recFlag of
    Just _  -> LetRecBinding allBindings body
    Nothing -> LetBinding allBindings body

parseIfExpr :: Parser Expr
parseIfExpr = do
  _       <- symbol "if"
  cond    <- parseExpr
  _       <- symbol "then"
  thn     <- parseExpr
  elseExp <- optional (symbol "else" *> parseExpr)
  return $ IfExpr cond thn elseExp

parseMatchExpr :: Parser Expr
parseMatchExpr = do
  _    <- symbol "match"
  expr <- parseExpr
  _    <- symbol "with"
  scn
  cases <- some parseMatchCase
  return $ MatchExpr expr cases
  where
    parseMatchCase :: Parser (Pattern, Expr)
    parseMatchCase = do
      scn
      _ <- optional (symbol "|" )  -- fixed: binding the result to _
      pat  <- parsePattern
      _    <- symbol "->"
      scn
      expr <- parseExpr
      return (pat, expr)

-- Allow chaining of type annotations.
withTypeAnnotation :: Expr -> Parser Expr
withTypeAnnotation e =
      (do { _ <- symbol ":"; t <- parseType; withTypeAnnotation (TypeAnnotation e t) })
  <|> return e

------------------------------------------------------------------------------
-- Operator Handling: Prefix and Binary Operators
------------------------------------------------------------------------------

-- Atom: the smallest unit.
atom :: Parser Expr
atom = choice
  [ parens parseExpr
  , parseLambdaExpr
  , parseLetExpr
  , parseIfExpr
  , parseMatchExpr
  , parseConstantExpr
  , parseIdentifierExpr
  ]

-- Prefix operators as a standalone parser.
prefixOperator :: Parser (Expr -> Expr)
prefixOperator = choice
  [ symbol "not" *> pure (unOp "not")
  , symbol "+"   *> pure (unOp "+")
  , symbol "-"   *> pure (unOp "-")
  ]

-- TERM: handles prefix operators and function application.
term :: Parser Expr
term = do
  preOps <- many (try prefixOperator)
  base   <- atom
  args   <- many (sc *> atom)
  let applied = if null args then base else Application base args
      expr    = foldr ($) applied preOps
  withTypeAnnotation expr

-- Helpers for binary and unary operator application.
binOp :: String -> Expr -> Expr -> Expr
binOp op x y = Application (Identifier op) [x, y]

unOp :: String -> Expr -> Expr
unOp op x = Application (Identifier ("U" ++ op)) [x]

-- Table for binary operators with their precedence.
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ binary "*"   (binOp "*")
    , binary "/"   (binOp "/")
    , binary "%"   (binOp "%")
    ]
  , [ binary "+"   (binOp "+")
    , binary "-"   (binOp "-")
    ]
  , [ binary ">"   (binOp ">")
    , binary ">="  (binOp ">=")
    , binary "<"   (binOp "<")
    , binary "<="  (binOp "<=")
    ]
  , [ binary "=="  (binOp "==")
    , binary "!="  (binOp "!=")
    ]
  , [ binary "&&"  (binOp "&&") ]
  , [ binary "||"  (binOp "||") ]
  ]
  where
    binary name f = InfixL (try (f <$ symbol name))

-- Main expression parser using the operator table.
parseExpr :: Parser Expr
parseExpr = makeExprParser term operatorTable

------------------------------------------------------------------------------
-- Top-Level Items and Programs
------------------------------------------------------------------------------

parseTopLevelLet :: Parser TopLevelItem
parseTopLevelLet = do
  _       <- symbol "let"
  recFlag <- optional (symbol "rec")
  firstB  <- parseBinding
  restBs  <- many (symboln "and" *> parseBinding)
  -- Allow an optional 'in' clause that we ignore in top-level let.
  _ <- optional (symboln "in" *> scn *> parseExpr)  -- fixed: bind the result to _
  lookAhead (void newline <|> eof)
  let allBindings = firstB : restBs
  return $ case recFlag of
    Just _  -> LetRecBindingItem allBindings
    Nothing -> LetBindingItem allBindings

parseTopLevelExpr :: Parser TopLevelItem
parseTopLevelExpr = do
  expr <- parseExpr
  lookAhead (void newline <|> eof)
  return (EvalExpr expr)

parseTopLevelItem :: Parser TopLevelItem
parseTopLevelItem =
  (lookAhead (symbol "let") *> parseTopLevelLet)
  <|> parseTopLevelExpr

parseProgram :: Parser [TopLevelItem]
parseProgram = scn *> sepEndBy parseTopLevelItem (some newline) <* eof
