{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( parseProgram,
    Parser,
  )
where

import AST
import Control.Monad (void)
import qualified Control.Monad.Combinators.Expr as E
import Data.Maybe (fromMaybe)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Our parser type – parsing a String with no custom error type.
type Parser = Parsec Void String

-- * Lexical Helpers

-- | Consume whitespace and OCaml-style nested comments.
sc :: Parser ()
sc = L.space space1 (L.skipBlockCommentNested "(*" "*)") empty

-- | Parse a lexeme and consume trailing space.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a fixed string and consume trailing space.
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parse something between parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parse something between brackets.
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- * Identifiers and Operators

reservedWords :: [String]
reservedWords =
  [ "true",
    "false",
    "match",
    "with",
    "let",
    "rec",
    "and",
    "in",
    "type",
    "function",
    "fun",
    "if",
    "then",
    "else"
  ]

-- | A general identifier (for variables, etc.) that is not a keyword.
identifier :: Parser String
identifier = lexeme . try $ do
  x <- (:) <$> (letterChar <|> char '_')
           <*> many (alphaNumChar <|> char '_' <|> char '\'')
  if x `elem` reservedWords
    then fail $ "keyword " ++ show x ++ " cannot be used as an identifier"
    else return x

-- | A capitalized identifier (used for constructors).
capitalized :: Parser String
capitalized = lexeme . try $ do
  x <- (:) <$> upperChar <*> many (alphaNumChar <|> char '_' <|> char '\'')
  if x `elem` reservedWords
    then fail $ "keyword " ++ show x ++ " cannot be used as a constructor"
    else return x

-- | An operator identifier.
operator :: Parser String
operator = lexeme . try $ do
  first <- satisfy (`elem` ("$&*+-/=<>@^|%~!?:." :: String))
  rest  <- many (satisfy (`elem` ("$&*+-/=<>@^|%~!?:." :: String)))
  let op = first : rest
  if op `elem` ["|", "->"]
    then fail "operator reserved as keyword"
    else return op

-- | A "value" identifier: either a normal identifier or a parenthesized operator.
valueIdentifier :: Parser String
valueIdentifier = try identifier <|> parens operator

-- * Constants

pConstant :: Parser Constant
pConstant =
  choice
    [ pInt,
      pChar,
      pString
    ]
  where
    pInt = do
      n <- lexeme L.decimal
      return $ IntConst n
    pChar = do
      void $ char '\''
      c <- anySingle
      void $ char '\''
      return $ CharConst c
    pString = do
      void $ char '"'
      s <- manyTill L.charLiteral (char '"')
      return $ StringConst s

-- * Type Expressions

pTypeExpr :: Parser TypeExpr
pTypeExpr = E.makeExprParser pTypeTerm typeOperators

pTypeTerm :: Parser TypeExpr
pTypeTerm =
  choice
    [ TypeVar <$> (char '\'' *> identifier),
      try $ do
        con <- identifier
        args <- optional (parens (pTypeExpr `sepBy1` symbol ","))
        return $ TypeConstructor con (fromMaybe [] args),
      parens pTypeExpr
    ]

typeOperators :: [[E.Operator Parser TypeExpr]]
typeOperators =
  [ [ E.InfixR (symbol "->" >> return TypeFunc)
    ],
    [ E.InfixN (symbol "*" >> return (\x y -> TypeTuple [x, y]))
    ]
  ]

-- * Patterns

pPattern :: Parser Pattern
pPattern = E.makeExprParser pPatternTerm patternOperators

pPatternTerm :: Parser Pattern
pPatternTerm =
  choice
    [ Wildcard <$ symbol "_",
      try (ConstPattern <$> pConstant),
      try $ do
          con <- capitalized
          mpat <- optional pPatternTerm
          return $ ConstructPattern con mpat,
      VarPattern <$> identifier,
      parens pPattern,
      brackets (sepBy pPattern (symbol ";")) >>= \ps ->
        return $
          foldr
            (\p acc -> ConstructPattern "::" (Just (TuplePattern [p, acc])))
            (ConstructPattern "[]" Nothing)
            ps
    ]

patternOperators :: [[E.Operator Parser Pattern]]
patternOperators =
  [ [E.InfixL (OrPattern <$ symbol "|")]
  ]

-- * Expressions

-- Add a simple parser for boolean literals.
pBool :: Parser Expr
pBool = do
  b <- lexeme (string "true" <|> string "false")
  return (Identifier b)

-- | Parse an atom: a basic expression unit.
pAtom :: Parser Expr
pAtom =
  choice
    [ try pIf,      -- if-expression is tried first
      try pLet,     -- let-expression (with "in")
      try pLambda,  -- lambda-expression
      try pMatch,   -- match-expression
      pBool,        -- boolean literals "true" or "false"
      try $ do      -- constructor application
          con <- capitalized
          mexpr <- optional pAtom
          return $ ConstructorExpr con mexpr,
      ConstantExpr <$> pConstant,
      Identifier <$> valueIdentifier,
      parens pExpr,
      -- List literal: use pTerm for items to avoid conflicts with infix ";"
      brackets (sepBy pTerm (symbol ";")) >>= \es ->
        return $
          foldr
            (Application . Application (Identifier "::"))
            (ConstructorExpr "[]" Nothing)
            es
    ]

-- | Parse a term by combining a sequence of atoms into left-associative application.
pTerm :: Parser Expr
pTerm = foldl Application <$> pAtom <*> many pAtom

-- | A postfix parser for a type annotation: [expr : ty].
postfixTypeAnnotation :: Parser (Expr -> Expr)
postfixTypeAnnotation = do
  void $ symbol ":"
  ty <- pTypeExpr
  return (`TypeAnnotation` ty)

-- | Parse a term with optional type annotations.
pTermWithAnnotation :: Parser Expr
pTermWithAnnotation = do
  expr <- pTerm
  as <- many postfixTypeAnnotation
  return $ foldl (\e f -> f e) expr as

-- | Parse expressions using an operator table.
pExpr :: Parser Expr
pExpr = E.makeExprParser pTermWithAnnotation exprOperators

exprOperators :: [[E.Operator Parser Expr]]
exprOperators =
  [ [ E.Prefix (do { void (symbol "!"); return (Application (Identifier "!")) }),
      E.Prefix (do
                  op <- choice [symbol "-", symbol "+"]
                  return (Application (Identifier ("~" ++ op)))
                )
    ],
    [ E.InfixR (do { op <- symbol "**"; return (Application . Application (Identifier op)) })
    ],
    [ E.InfixL (do { op <- choice [symbol "*", symbol "/", symbol "%"]; return (Application . Application (Identifier op)) })
    ],
    [ E.InfixL (do { op <- choice [symbol "+", symbol "-"]; return (Application . Application (Identifier op)) })
    ],
    [ E.InfixR (do { void (symbol "::"); return (\x y -> ConstructorExpr "::" (Just (TupleExpr [x, y]))) })
    ],
    [ E.InfixL (do { op <- choice [ symbol "=",
                                      symbol "<",
                                      symbol ">",
                                      symbol "|",
                                      symbol "&",
                                      symbol "$"
                                    ]
                   ; return (Application . Application (Identifier op))
                 })
    ],
    [ E.InfixR (do { op <- symbol "&&"; return (Application . Application (Identifier op)) })
    ],
    [ E.InfixR (do { op <- symbol "||"; return (Application . Application (Identifier op)) })
    ],
    [ E.InfixN (do { void (symbol ","); return (\x y -> TupleExpr [x, y]) })
    ],
    [ E.InfixN (do { void (symbol ";")
                   ; return (\x y ->
                              case x of
                                SequenceExpr xs -> SequenceExpr (xs ++ [y])
                                _ -> SequenceExpr [x, y])
                 })
    ]
  ]

-- * Let, Lambda, Match, and If Expressions

-- | Parse a let-expression (with an "in" clause), e.g. [let x = 42 in x].
pLet :: Parser Expr
pLet = do
  void $ symbol "let"
  isRec <- (True <$ symbol "rec") <|> return False
  bindings <- pBinding `sepBy1` symbol "and"
  void $ symbol "in"
  body <- pExpr
  return $ if isRec then LetRecBinding bindings body
                    else LetBinding bindings body

pBinding :: Parser (Pattern, Expr)
pBinding = do
  pat <- pPattern
  -- Use a simpler parser for parameters (simple identifiers)
  params <- many (VarPattern <$> identifier)
  void $ symbol "="
  expr <- pExpr
  let rhs = if null params then expr else Lambda params expr
  return (pat, rhs)

-- | Parse a lambda-expression: [fun P1 P2 ... -> E].
pLambda :: Parser Expr
pLambda = do
  void $ symbol "fun"
  args <- some pPattern
  void $ symbol "->"
  Lambda args <$> pExpr

-- | Parse a match-expression: [match E with | P1 -> E1 | ...].
pMatch :: Parser Expr
pMatch = do
  void $ symbol "match"
  expr <- pExpr
  void $ symbol "with"
  cases <- some pCase
  return $ MatchExpr expr cases

pCase :: Parser (Pattern, Expr)
pCase = do
  void $ optional (symbol "|")
  pat <- pPattern
  void $ symbol "->"
  expr <- pExpr
  return (pat, expr)

-- | Parse an if-expression: [if E then E [else E]].
pIf :: Parser Expr
pIf = try $ do
  void $ symbol "if"
  cond <- pExpr
  void $ symbol "then"
  trueBranch <- pExpr
  elseBranch <- optional (symbol "else" *> pExpr)
  return $ IfExpr cond trueBranch elseBranch

-- * Top-Level Items

-- | Top-level items: type definitions, let-expressions, or evaluation expressions.
pTopLevel :: Parser TopLevelItem
pTopLevel =
  choice
    [ try pTypeDef,
      try (EvalExpr <$> pLet), 
      try pLetTop,
      EvalExpr <$> pExpr
    ]

-- | Parse a type definition.
pTypeDef :: Parser TopLevelItem
pTypeDef = do
  void $ symbol "type"
  params <- many (char '\'' *> identifier)
  tyId   <- identifier
  void $ symbol "="
  variants <- pConstructorDecl `sepBy1` symbol "|"
  return $ TypeDef (TypeDeclaration tyId params variants)

pConstructorDecl :: Parser ConstructorDecl
pConstructorDecl = do
  con <- capitalized
  mType <- optional (symbol "of" *> pTypeExpr)
  return $ ConstructorDecl con mType

-- | Parse a top-level let binding (without an "in" clause).
-- If "in" follows, it is parsed as a let-expression.
pLetTop :: Parser TopLevelItem
pLetTop = try $ do
  void $ symbol "let"
  isRec <- (True <$ symbol "rec") <|> return False
  bindings <- pBinding `sepBy1` symbol "and"
  notFollowedBy (lookAhead (symbol "in"))
  return $ if isRec then LetRecBindingItem bindings else LetBindingItem bindings

-- | A program is a sequence of top-level items (optionally separated by ";;").
pProgram :: Parser Program
pProgram = sc *> many (pTopLevel <* optional (symbol ";;")) <* eof

-- * Exported Entry Point

-- | Parse an entire program from a String.
parseProgram :: String -> Either (ParseErrorBundle String Void) Program
parseProgram = runParser pProgram ""
