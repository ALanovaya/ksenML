{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST
import Data.Void
import Control.Monad (void)
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr (Operator, makeExprParser, Operator(Prefix), Operator(InfixL))
import qualified Text.Megaparsec.Char.Lexer as L

-- Define the parser type: we work with strings and use Void for errors
type Parser = Parsec Void String

-- Space consumer: skips whitespace
sc :: Parser ()
sc = skipMany (char ' '  <|> char '\t')

-- Lexeme and symbol parsers that take whitespace into account
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- Wrapper for parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- List of reserved words (they cannot be used as identifiers)
reservedWords :: [String]
reservedWords =
  [ "let", "in", "if", "then", "else", "fun", "rec"
  , "match", "with", "true", "false", "int", "bool", "and"
  ]

-- Identifier parser: starts with a letter or '_' and then letters, digits, '_' or '''
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> (letterChar <|> char '_')
            <*> many (alphaNumChar <|> oneOf ("_'" :: String))
    check x = if x `elem` reservedWords
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

-- Parser for constants: integer and boolean literals
parseConstant :: Parser Constant
parseConstant = IntConst <$> lexeme L.decimal
            <|> BoolConst True  <$ symbol "true"
            <|> BoolConst False <$ symbol "false"

-- Parser for types
parseType :: Parser TypeExpr
parseType = makeFunctionType
  where
    parseTypeAtom =
           (symbol "int"  >> return TypeInt)
       <|> (symbol "bool" >> return TypeBool)
       <|> TypeVar <$> identifier
       <|> parens parseType
    -- Function types are right-associative and use "->" as the arrow
    makeFunctionType = do
      t <- parseTypeAtom
      rest <- optional (symbol "->" *> parseType)
      case rest of
        Just t' -> return (TypeFunc t t')
        Nothing -> return t

-- Parser for patterns
parsePattern :: Parser Pattern
parsePattern = do
  p <- choice
         [ Wildcard <$ symbol "_"                           -- _ → Wildcard
         , ConstPattern <$> try parseConstant                -- constant → ConstPattern
         , VarPattern <$> identifier                        -- identifier → VarPattern
         , parens parsePattern                              -- parentheses
         ]
  -- If a type annotation follows the pattern, wrap it in TypePattern
  option p (do { _ <- symbol ":"; TypePattern p <$> parseType; })

-- Parsers for expressions
parseConstantExpr :: Parser Expr
parseConstantExpr = ConstantExpr <$> parseConstant

parseIdentifierExpr :: Parser Expr
parseIdentifierExpr = Identifier <$> identifier

parseLambdaExpr :: Parser Expr
parseLambdaExpr = do
  _    <- symbol "fun"
  pats <- some parsePattern
  _    <- symbol "->"
  Lambda pats <$> parseExpr

parseBinding :: Parser (Pattern, Expr)
parseBinding = do
  pat  <- parsePattern
  _    <- symbol "="
  expr <- parseExpr
  return (pat, expr)

parseLetExpr :: Parser Expr
parseLetExpr = do
  _       <- symbol "let"
  recFlag <- optional (symbol "rec")
  binding <- parseBinding
  bindings <- many (symbol "and" *> parseBinding)
  _       <- symbol "in"
  body    <- parseExpr
  let allBindings = binding : bindings
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
  _     <- symbol "match"
  expr  <- parseExpr
  _     <- symbol "with"
  cases <- some parseCase
  return $ MatchExpr expr cases
  where
    parseCase = do
      optional (symbol "|")
      pat  <- parsePattern
      _    <- symbol "->"
      expr <- parseExpr
      return (pat, expr)

-- Postfix handling for type annotations in expressions
withTypeAnnotation :: Expr -> Parser Expr
withTypeAnnotation e =
      (do { _ <- symbol ":"; t <- parseType; withTypeAnnotation (TypeAnnotation e t) })
  <|> return e

-- Factor – the basic unit without function application
factor :: Parser Expr
factor = choice
  [ parens parseExpr
  , parseLambdaExpr
  , parseLetExpr
  , parseIfExpr
  , parseMatchExpr
  , parseConstantExpr
  , parseIdentifierExpr
  ]

-- Term – a factor with possible function application chain and postfix type annotations
term :: Parser Expr
term = do
  f    <- factor
  args <- many factor
  let app = if null args then f else Application f args
  withTypeAnnotation app

-- Operators
-- Function for binary operators: the result is an application of the operator as a function.
binOp :: String -> Expr -> Expr -> Expr
binOp op x y = Application (Identifier op) [x, y]

-- Function for unary operators: the result is an application of the operator (prefixed with "U") to the argument.
unOp :: String -> Expr -> Expr
unOp op x = Application (Identifier ("U" ++ op)) [x]

-- Define the operator table with appropriate precedences and associativity
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "not" (unOp "not")
    , prefix "+"   (unOp "+")
    , prefix "-"   (unOp "-")
    ]
  , [ binary "*"   (binOp "*")
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
    prefix name f = Prefix (try (f <$ symbol name))

-- The main expression parser using the operator table
parseExpr :: Parser Expr
parseExpr = makeExprParser term operatorTable

-- Parsers for top-level constructs
parseTopLevelItem :: Parser TopLevelItem
parseTopLevelItem =
      try (do
         _       <- symbol "let"
         recFlag <- optional (symbol "rec")
         binding <- parseBinding
         bindings <- many (symbol "and" *> parseBinding)
         let allBindings = binding : bindings
         return $ case recFlag of
           Just _  -> LetRecBindingItem allBindings
           Nothing -> LetBindingItem allBindings)
  <|> EvalExpr <$> parseExpr

-- Parser for a program: a sequence of top-level items until end of input
parseProgram :: Parser [TopLevelItem]
parseProgram = sepEndBy parseTopLevelItem newline <* eof
