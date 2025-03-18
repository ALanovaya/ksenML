{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Parser
import AST
import Text.Megaparsec (parse, errorBundlePretty)

-- Helper function to test successful parsing
parseSuccess :: (Eq a, Show a) => Parser a -> String -> a -> Expectation
parseSuccess p input expected =
  case parse p "" input of
    Right result -> result `shouldBe` expected
    Left err     -> expectationFailure (errorBundlePretty err)

-- Helper function to test that parsing fails
parseFailure :: Show a => Parser a -> String -> Expectation
parseFailure p input =
  case parse p "" input of
    Left _  -> return ()
    Right r -> expectationFailure ("Expected an error, but got: " ++ show r)

-----------------------------------------------------------
-- QuickCheck: Arbitrary Instances for AST Types
-----------------------------------------------------------

----------------------------------------------------------------------
-- Generators for Basic Components
----------------------------------------------------------------------

-- Generator for identifiers (avoiding reserved words)
genIdentifier :: Gen String
genIdentifier = elements ["x", "y", "z", "foo", "bar"]

-- Generator for constants.
genConstant :: Gen String
genConstant = oneof [ show <$> choose (-100 :: Int, 100 :: Int)
                    , return "true"
                    , return "false"
                    ]

-- Generator for type expressions as strings.
genType :: Int -> Gen String
genType n | n <= 1 = elements ["int", "bool", "a", "b", "c"]
          | otherwise = frequency
              [ (3, genType 1)
              , (1, do t1 <- genType (n `div` 2)
                       t2 <- genType (n `div` 2)
                       return (t1 ++ " -> " ++ t2))
              ]

-- Generator for patterns.
genPattern :: Int -> Gen String
genPattern n | n <= 1 = oneof [ return "_"       -- wildcard
                              , genIdentifier    -- variable pattern
                              , genConstant      -- constant pattern
                              ]
             | otherwise = frequency
              [ (3, return "_")
              , (3, genIdentifier)
              , (2, genConstant)
              , (2, do pat <- oneof [genIdentifier, genConstant]
                       tp  <- genType (n `div` 2)
                       return (pat ++ " : " ++ tp))
              ]

----------------------------------------------------------------------
-- Generator for Expressions
----------------------------------------------------------------------
-- We write a recursive generator (using the current size parameter)
-- that produces source strings for expressions matching grammar.
genExpr :: Int -> Gen String
genExpr n | n <= 1 = oneof [ genConstant, genIdentifier ]
          | otherwise = frequency
            [ (3, genConstant)
            , (3, genIdentifier)
            , (2, genLambda n)
            , (2, genLet n)
            , (2, genIf n)
            , (2, genMatch n)
            , (2, genApp n)
            , (2, genTypeAnn n)
            ]

-- Lambda: "fun <pattern>+ -> <expr>"
genLambda :: Int -> Gen String
genLambda n = do
  pats <- listOf1 (resize (n `div` 2) (genPattern (n `div` 2))) `suchThat` (\xs -> length xs <= 2)
  body <- resize (n - 1) (genExpr (n - 1))
  return $ "fun " ++ unwords pats ++ " -> " ++ body

-- Let expression: "let [rec] <pattern> = <expr> [and <pattern> = <expr> ...] in <expr>"
genLet :: Int -> Gen String
genLet n = do
  recFlag <- frequency [(1, return "rec "), (3, return "")]
  pat <- resize (n `div` 2) (genPattern (n `div` 2))
  rhs <- resize (n - 1) (genExpr (n - 1))
  body <- resize (n - 1) (genExpr (n - 1))
  andBindings <- resize (n - 1) (listOf (do
                           pat' <- resize (n `div` 2) (genPattern (n `div` 2))
                           expr' <- resize (n - 1) (genExpr (n - 1))
                           return ("and " ++ pat' ++ " = " ++ expr')))
                    `suchThat` (\xs -> length xs <= 2)
  return $ "let " ++ recFlag ++ pat ++ " = " ++ rhs ++ " " ++ unwords andBindings ++ " in " ++ body

-- If expression: "if <expr> then <expr> [else <expr>]"
genIf :: Int -> Gen String
genIf n = do
  cond <- resize (n - 1) (genExpr (n - 1))
  thn <- resize (n - 1) (genExpr (n - 1))
  els <- frequency [(1, resize (n - 1) (genExpr (n - 1))), (3, return "")]
  if els == ""
    then return $ "if " ++ cond ++ " then " ++ thn
    else return $ "if " ++ cond ++ " then " ++ thn ++ " else " ++ els

-- Match expression: "match <expr> with | <pattern> -> <expr> [| ...]"
genMatch :: Int -> Gen String
genMatch n = do
  e <- resize (n - 1) (genExpr (n - 1))
  numCases <- choose (1, 3)
  cases <- vectorOf numCases (genCase (n `div` 2))
  return $ "match " ++ e ++ " with " ++ unwords cases

genCase :: Int -> Gen String
genCase n = do
  pat <- resize n (genPattern n)
  expr <- resize (n - 1) (genExpr (n - 1))
  return $ "| " ++ pat ++ " -> " ++ expr

-- Function application: <expr> <expr> [<expr> ...]
genApp :: Int -> Gen String
genApp n = do
  f <- resize (n - 1) (genExpr (n - 1))
  numArgs <- choose (1, 3)
  args <- vectorOf numArgs (resize (n - 1) (genExpr (n - 1)))
  return $ f ++ " " ++ unwords args

-- Type annotation: <expr> : <type>
genTypeAnn :: Int -> Gen String
genTypeAnn n = do
  e <- resize (n - 1) (genExpr (n - 1))
  t <- resize (n `div` 2) (genType (n `div` 2))
  return $ e ++ " : " ++ t

----------------------------------------------------------------------
-- Generator for Top-Level Items and Programs
----------------------------------------------------------------------
-- A top-level item is either a let binding or an expression.
genTopLevelItem :: Int -> Gen String
genTopLevelItem n = oneof [ genLet n, genExpr n ]

-- A program is one or more top-level items separated by newlines.
genProgram :: Int -> Gen String
genProgram n = do
  numItems <- choose (1, 5)
  items <- vectorOf numItems (genTopLevelItem n)
  return $ unlines items

----------------------------------------------------------------------
-- QuickCheck Property: Parsing Generated Programs
----------------------------------------------------------------------
-- This property asserts that every generated program string should be
-- successfully parsed by parser.
prop_parseProgram :: Property
prop_parseProgram =
  forAll (sized genProgram) $ \input ->
    case parse parseProgram "" input of
      Right _ -> property True
      Left err ->
        counterexample ("Failed parsing:\n" ++ input ++ "\nError:\n" ++ show err) $
          property False

-----------------------------------------------------------
-- Main: Hspec Unit Tests and QuickCheck Property Tests
-----------------------------------------------------------

main :: IO ()
main = hspec $ do

  -------------------------------------------------------------------------------
  -- Basic Parsers
  -------------------------------------------------------------------------------
  describe "Basic Parsers" $ do

    describe "Constant Parsing" $ do
      it "parses an integer constant" $ do
        parseSuccess parseConstant "42" (IntConst 42)
      
      it "parses a boolean constant (true)" $ do
        parseSuccess parseConstant "true" (BoolConst True)
      
      it "parses a boolean constant (false)" $ do
        parseSuccess parseConstant "false" (BoolConst False)
    
    describe "Identifier Parsing" $ do
      it "parses a valid identifier" $ do
        parseSuccess identifier "varName" "varName"
      
      it "rejects reserved words" $ do
        parseFailure identifier "if"

    describe "Type Parsing" $ do
      it "parses basic types" $ do
        parseSuccess parseType "int"  TypeInt
        parseSuccess parseType "bool" TypeBool
        parseSuccess parseType "a"    (TypeVar "a")
      
      it "parses a function type" $ do
        let expected = TypeFunc TypeInt TypeBool
        parseSuccess parseType "int -> bool" expected
      
      it "parses nested function types (right-associative)" $ do
        let expected = TypeFunc TypeInt (TypeFunc TypeBool TypeInt)
        parseSuccess parseType "int -> bool -> int" expected

    describe "Pattern Parsing" $ do
      it "parses a wildcard pattern" $ do
        parseSuccess parsePattern "_" Wildcard
      
      it "parses a variable pattern" $ do
        parseSuccess parsePattern "x" (VarPattern "x")
      
      it "parses a constant pattern" $ do
        parseSuccess parsePattern "42" (ConstPattern (IntConst 42))
      
      it "parses a pattern with type annotation" $ do
        parseSuccess parsePattern "x : int" (TypePattern (VarPattern "x") TypeInt)
      
      it "parses nested parentheses in a pattern: ((x))" $ do
        parseSuccess parsePattern "((x))" (VarPattern "x")

  -------------------------------------------------------------------------------
  -- Expression Parsers
  -------------------------------------------------------------------------------
  describe "Expression Parsers" $ do

    ---------------------------------------------------------------------------
    -- Lambda Expressions
    ---------------------------------------------------------------------------
    describe "Lambda Expressions" $ do
      it "parses a simple lambda expression" $ do
        let input = "fun x -> x"
            expected = Lambda [VarPattern "x"] (Identifier "x")
        parseSuccess parseLambdaExpr input expected

      it "parses lambda with multiple parameters" $ do
        let input = "fun x y -> x + y"
            expected = Lambda [VarPattern "x", VarPattern "y"]
                        (Application (Identifier "+")
                          [ Identifier "x"
                          , Identifier "y"
                          ])
        parseSuccess parseLambdaExpr input expected

      it "parses lambda with annotated parameter" $ do
        let input = "fun (x : int) -> x + 1"
            expected = Lambda [TypePattern (VarPattern "x") TypeInt]
                        (Application (Identifier "+")
                          [ Identifier "x"
                          , ConstantExpr (IntConst 1)
                          ])
        parseSuccess parseLambdaExpr input expected

      it "parses a lambda with a constant pattern parameter" $ do
        let input = "fun 42 -> 1"
            expected = Lambda [ConstPattern (IntConst 42)] (ConstantExpr (IntConst 1))
        parseSuccess parseLambdaExpr input expected

      it "parses a lambda with a parenthesized parameter" $ do
        let input = "fun (x) -> x"
            expected = Lambda [VarPattern "x"] (Identifier "x")
        parseSuccess parseLambdaExpr input expected

      it "parses a lambda whose body is a type-annotated expression: fun x -> x : int" $ do
        let input = "fun x -> x : int"
            expected = Lambda [VarPattern "x"] (TypeAnnotation (Identifier "x") TypeInt)
        parseSuccess parseLambdaExpr input expected

      it "parses lambda with multiple annotated parameters" $ do
        let input = "fun (x : int) (y : bool) -> if y then x else x + 1"
            expected = Lambda [ TypePattern (VarPattern "x") TypeInt
                              , TypePattern (VarPattern "y") TypeBool ]
                         (IfExpr (Identifier "y")
                           (Identifier "x")
                           (Just (Application (Identifier "+")
                                  [Identifier "x", ConstantExpr (IntConst 1)])))
        parseSuccess parseLambdaExpr input expected

      it "parses nested lambda with let expression inside" $ do
        let input = "fun a b -> let c = a + b in c * c"
            expected = Lambda [VarPattern "a", VarPattern "b"]
                        (LetBinding [(VarPattern "c", Application (Identifier "+") [Identifier "a", Identifier "b"])]
                          (Application (Identifier "*") [Identifier "c", Identifier "c"]))
        parseSuccess parseLambdaExpr input expected

    ---------------------------------------------------------------------------
    -- Let Expressions
    ---------------------------------------------------------------------------
    describe "Let Expressions" $ do
      it "parses a standard let expression" $ do
        let input = "let x = 1 in x"
            expected = LetBinding [(VarPattern "x", ConstantExpr (IntConst 1))] (Identifier "x")
        parseSuccess parseLetExpr input expected
      
      it "parses a recursive let expression" $ do
        let input = "let rec f = fun x -> x in f"
            expected = LetRecBinding [(VarPattern "f", Lambda [VarPattern "x"] (Identifier "x"))] (Identifier "f")
        parseSuccess parseLetExpr input expected
      
      it "parses a let expression with multiple bindings" $ do
        let input = "let x = 1 and y = 2 in x"
            expected = LetBinding [(VarPattern "x", ConstantExpr (IntConst 1)),
                                   (VarPattern "y", ConstantExpr (IntConst 2))]
                                  (Identifier "x")
        parseSuccess parseLetExpr input expected
      
      it "fails to parse a let expression missing a proper binding" $ do
        parseFailure parseLetExpr "let x = in x"

    ---------------------------------------------------------------------------
    -- If Expressions
    ---------------------------------------------------------------------------
    describe "If Expressions" $ do
      it "parses an if-then-else expression" $ do
        let input = "if true then 1 else 0"
            expected = IfExpr (ConstantExpr (BoolConst True))
                              (ConstantExpr (IntConst 1))
                              (Just (ConstantExpr (IntConst 0)))
        parseSuccess parseIfExpr input expected
      
      it "parses an if-then expression without else" $ do
        let input = "if false then 42"
            expected = IfExpr (ConstantExpr (BoolConst False))
                              (ConstantExpr (IntConst 42))
                              Nothing
        parseSuccess parseIfExpr input expected
      
      it "parses nested if expressions" $ do
        let input = "if true then if false then 1 else 2 else 3"
            expected = IfExpr (ConstantExpr (BoolConst True))
                        (IfExpr (ConstantExpr (BoolConst False))
                          (ConstantExpr (IntConst 1))
                          (Just (ConstantExpr (IntConst 2))))
                        (Just (ConstantExpr (IntConst 3)))
        parseSuccess parseIfExpr input expected
      
      it "fails on an incomplete if expression: if true then" $ do
        parseFailure parseIfExpr "if true then"
      
      it "parses if expression with complex binary conditions" $ do
        let input = "if a * b == c then a + b else a - b"
            expected = IfExpr (Application (Identifier "==")
                                      [ Application (Identifier "*") [Identifier "a", Identifier "b"]
                                      , Identifier "c" ])
                              (Application (Identifier "+") [Identifier "a", Identifier "b"])
                              (Just (Application (Identifier "-") [Identifier "a", Identifier "b"]))
        parseSuccess parseIfExpr input expected

    ---------------------------------------------------------------------------
    -- Match Expressions
    ---------------------------------------------------------------------------
    describe "Match Expressions" $ do
      it "parses a simple match expression" $ do
        let input = "match x with | _ -> 1"
            expected = MatchExpr (Identifier "x") [(Wildcard, ConstantExpr (IntConst 1))]
        parseSuccess parseMatchExpr input expected
      
      it "parses match expression with multiple cases" $ do
        let input = "match x with | 0 -> 1 | _ -> 2"
            expected = MatchExpr (Identifier "x")
                        [ (ConstPattern (IntConst 0), ConstantExpr (IntConst 1))
                        , (Wildcard, ConstantExpr (IntConst 2))
                        ]
        parseSuccess parseMatchExpr input expected
      
      it "fails on a malformed match expression: match x with | -> 1" $ do
        parseFailure parseMatchExpr "match x with | -> 1"

    ---------------------------------------------------------------------------
    -- General Expression Parsing
    ---------------------------------------------------------------------------
    describe "General Expression Parsing" $ do
      it "parses function application" $ do
        let input = "f 1 2"
            expected = Application (Identifier "f")
                                   [ConstantExpr (IntConst 1), ConstantExpr (IntConst 2)]
        parseSuccess parseExpr input expected
      
      it "parses an expression with binary operators respecting precedence" $ do
        let input = "1 + 2 * 3"
            expected = Application (Identifier "+")
                          [ ConstantExpr (IntConst 1)
                          , Application (Identifier "*")
                              [ ConstantExpr (IntConst 2)
                              , ConstantExpr (IntConst 3)
                              ]
                          ]
        parseSuccess parseExpr input expected
      
      it "parses type annotation in an expression" $ do
        let input = "1 : int"
            expected = TypeAnnotation (ConstantExpr (IntConst 1)) TypeInt
        parseSuccess parseExpr input expected
      
      it "parses function application with parenthesized expression" $ do
        let input = "f (1 + 2)"
            expected = Application (Identifier "f")
                        [ Application (Identifier "+")
                            [ ConstantExpr (IntConst 1)
                            , ConstantExpr (IntConst 2)
                            ]
                        ]
        parseSuccess parseExpr input expected
      
      it "fails on malformed function type" $ do
        parseFailure parseType "int ->"
      
      it "fails on incomplete type annotation in pattern" $ do
        parseFailure parsePattern "x :"
      
      it "fails on a malformed binary expression: 1 + * 2" $ do
        parseFailure parseExpr "1 + * 2"
      
      it "parses multiple function application" $ do
        let input = "f 42 43 44"
            expected = [ EvalExpr (Application (Identifier "f")
                                [ ConstantExpr (IntConst 42)
                                , ConstantExpr (IntConst 43)
                                , ConstantExpr (IntConst 44)]) ]
        parseSuccess parseProgram input expected
      
      it "parses expressions with multiple binary operators respecting precedence" $ do
        let input = "1 + 2 * 3 - 4"
            expected = [ EvalExpr (Application (Identifier "-")
                                [ Application (Identifier "+")
                                    [ ConstantExpr (IntConst 1)
                                    , Application (Identifier "*")
                                        [ ConstantExpr (IntConst 2)
                                        , ConstantExpr (IntConst 3)
                                        ]
                                    ]
                                , ConstantExpr (IntConst 4)
                                ]) ]
        parseSuccess parseProgram input expected
      
      it "parses complex function application" $ do
        let input = "fix (fun self l -> map (fun li x -> li (self l) x) l) l"
            expected = [ EvalExpr (Application (Identifier "fix")
                                [ Lambda [VarPattern "self", VarPattern "l"]
                                    (Application (Identifier "map")
                                        [ Lambda [VarPattern "li", VarPattern "x"]
                                            (Application (Identifier "li")
                                                [ Application (Identifier "self") [Identifier "l"]
                                                , Identifier "x"
                                                ])
                                        , Identifier "l"
                                        ])
                                , Identifier "l"
                                ]) ]
        parseSuccess parseProgram input expected
      
      it "parses parenthesized type annotation on binary expression" $ do
        let input = "(1 + 2) : int"
            expected = TypeAnnotation (Application (Identifier "+")
                                               [ConstantExpr (IntConst 1), ConstantExpr (IntConst 2)])
                                      TypeInt
        parseSuccess parseExpr input expected
      
      it "parses expression with prefix 'not' and binary '&&': not a && b" $ do
        let input = "not a && b"
            expected = Application (Identifier "&&")
                        [ Application (Identifier "Unot") [Identifier "a"]
                        , Identifier "b"
                        ]
        parseSuccess parseExpr input expected
      
      it "parses expression with unary minus and multiplication: -3 * 4" $ do
        let input = "-3 * 4"
            expected = Application (Identifier "*")
                        [ Application (Identifier "U-") [ConstantExpr (IntConst 3)]
                        , ConstantExpr (IntConst 4)
                        ]
        parseSuccess parseExpr input expected
      
      it "parses chained unary operators: not not true" $ do
        let input = "not not true"
            expected = Application (Identifier "Unot")
                        [ Application (Identifier "Unot") [ConstantExpr (BoolConst True)]
                        ]
        parseSuccess parseExpr input expected
      
      it "parses mixed binary operators with precedence: a && b || c" $ do
        let input = "a && b || c"
            expected = Application (Identifier "||")
                        [ Application (Identifier "&&") [Identifier "a", Identifier "b"]
                        , Identifier "c"
                        ]
        parseSuccess parseExpr input expected
      
      it "parses left-associative binary operator chain: a - b - c" $ do
        let input = "a - b - c"
            expected = Application (Identifier "-")
                        [ Application (Identifier "-") [Identifier "a", Identifier "b"]
                        , Identifier "c"
                        ]
        parseSuccess parseExpr input expected
      
      it "parses a complex binary expression: a + b * c - d / e" $ do
        let input = "a + b * c - d / e"
            expected = Application (Identifier "-")
                        [ Application (Identifier "+")
                            [ Identifier "a"
                            , Application (Identifier "*") [Identifier "b", Identifier "c"]
                            ]
                        , Application (Identifier "/") [Identifier "d", Identifier "e"]
                        ]
        parseSuccess parseExpr input expected
      
      it "parses chained type annotations: 1 : int : bool" $ do
        let input = "1 : int : bool"
            expected = TypeAnnotation (TypeAnnotation (ConstantExpr (IntConst 1)) TypeInt) TypeBool
        parseSuccess parseExpr input expected
      
      it "parses type annotation on a parenthesized function application: (f 1) : bool" $ do
        let input = "(f 1) : bool"
            expected = TypeAnnotation (Application (Identifier "f") [ConstantExpr (IntConst 1)]) TypeBool
        parseSuccess parseExpr input expected

  -------------------------------------------------------------------------------
  -- Top-Level & Program Parsers
  -------------------------------------------------------------------------------
  describe "Top-Level & Program Parsers" $ do

    describe "Top-Level Items" $ do
      it "parses a top-level let binding" $ do
        let input = "let x = 1"
            expected = LetBindingItem [(VarPattern "x", ConstantExpr (IntConst 1))]
        parseSuccess parseTopLevelItem input expected
      
      it "parses an expression for evaluation" $ do
        let input = "42"
            expected = EvalExpr (ConstantExpr (IntConst 42))
        parseSuccess parseTopLevelItem input expected
      
      it "parses a top-level let binding with multiple 'and' clauses: let a = 10 and b = 20" $ do
        let input = "let a = 10 and b = 20\n"
            expected = LetBindingItem
                        [ (VarPattern "a", ConstantExpr (IntConst 10))
                        , (VarPattern "b", ConstantExpr (IntConst 20))
                        ]
        parseSuccess parseTopLevelItem input expected

    describe "Program Parsing" $ do
      it "parses a program with multiple top-level items" $ do
        let input = "let x = 1\nlet rec f = fun y -> y\nf x"
            expected = [ LetBindingItem [(VarPattern "x", ConstantExpr (IntConst 1))]
                       , LetRecBindingItem [(VarPattern "f", Lambda [VarPattern "y"] (Identifier "y"))]
                       , EvalExpr (Application (Identifier "f") [Identifier "x"])
                       ]
        parseSuccess parseProgram input expected

      it "parses a factorial function definition and evaluation" $ do
        let input = "let rec fact = fun n -> if n == 0 then 1 else n * fact (n - 1)\nfact 5"
            expected =
              [ LetRecBindingItem
                  [ ( VarPattern "fact"
                    , Lambda [VarPattern "n"]
                        (IfExpr
                          (Application (Identifier "==")
                            [ Identifier "n"
                            , ConstantExpr (IntConst 0)
                            ])
                          (ConstantExpr (IntConst 1))
                          (Just (Application (Identifier "*")
                                  [ Identifier "n"
                                  , Application (Identifier "fact")
                                      [ Application (Identifier "-")
                                          [ Identifier "n"
                                          , ConstantExpr (IntConst 1)
                                          ]
                                      ]
                                  ]))
                        )
                    )
                  ]
              , EvalExpr (Application (Identifier "fact")
                           [ ConstantExpr (IntConst 5) ])
              ]
        parseSuccess parseProgram input expected

      it "parses a complete program with mixed top-level items" $ do
        let input = unlines
              [ "let x = 10"
              , "let rec inc = fun y -> y + 1"
              , "inc x"
              ]
            expected =
              [ LetBindingItem [(VarPattern "x", ConstantExpr (IntConst 10))]
              , LetRecBindingItem [(VarPattern "inc", Lambda [VarPattern "y"]
                                    (Application (Identifier "+")
                                      [ Identifier "y"
                                      , ConstantExpr (IntConst 1)
                                      ]))]
              , EvalExpr (Application (Identifier "inc") [Identifier "x"])
              ]
        parseSuccess parseProgram input expected

      it "parses multiple function application" $ do
        let input = "f 42 43 44"
            expected = [ EvalExpr (Application (Identifier "f")
                                [ ConstantExpr (IntConst 42)
                                , ConstantExpr (IntConst 43)
                                , ConstantExpr (IntConst 44)]) ]
        parseSuccess parseProgram input expected

      it "parses expressions with multiple binary operators respecting precedence" $ do
        let input = "1 + 2 * 3 - 4"
            expected = [ EvalExpr (Application (Identifier "-")
                                [ Application (Identifier "+")
                                    [ ConstantExpr (IntConst 1)
                                    , Application (Identifier "*")
                                        [ ConstantExpr (IntConst 2)
                                        , ConstantExpr (IntConst 3)
                                        ]
                                    ]
                                , ConstantExpr (IntConst 4)
                                ]) ]
        parseSuccess parseProgram input expected

      it "parses complex function application" $ do
        let input = "fix (fun self l -> map (fun li x -> li (self l) x) l) l"
            expected = [ EvalExpr (Application (Identifier "fix")
                                [ Lambda [VarPattern "self", VarPattern "l"]
                                    (Application (Identifier "map")
                                        [ Lambda [VarPattern "li", VarPattern "x"]
                                            (Application (Identifier "li")
                                                [ Application (Identifier "self") [Identifier "l"]
                                                , Identifier "x"
                                                ])
                                        , Identifier "l"
                                        ])
                                , Identifier "l"
                                ]) ]
        parseSuccess parseProgram input expected

      it "parses nested let expressions" $ do
        let input = "let x = 1 in let y = 2 in x + y"
            expected = LetBinding [(VarPattern "x", ConstantExpr (IntConst 1))]
                        (LetBinding [(VarPattern "y", ConstantExpr (IntConst 2))]
                          (Application (Identifier "+")
                            [ Identifier "x"
                            , Identifier "y"
                            ]))
        parseSuccess parseLetExpr input expected

      it "parses a program defining compose and square functions" $ do
        let input = unlines
              [ "let compose = fun f g x -> f (g x)"
              , "let square = fun x -> x * x"
              , "compose square square 3"
              ]
            expected = [ LetBindingItem
                         [ (VarPattern "compose",
                            Lambda [VarPattern "f", VarPattern "g", VarPattern "x"]
                              (Application (Identifier "f")
                                [ Application (Identifier "g") [Identifier "x"] ]))
                         ]
                       , LetBindingItem
                         [ (VarPattern "square",
                            Lambda [VarPattern "x"]
                              (Application (Identifier "*")
                                [Identifier "x", Identifier "x"]))
                         ]
                       , EvalExpr (Application (Identifier "compose")
                                   [Identifier "square", Identifier "square", ConstantExpr (IntConst 3)])
                       ]
        parseSuccess parseProgram input expected

      it "parses a program with nested let expressions and function application" $ do
        let input = unlines
              [ "let x = 10 and y = 20 and z = 30 in x * y + z"
              , "let rec process = fun n ->"
              , "  if n == 0 then 0 else process (n - 1) + n"
              , "process 5"
              ]
            expected = [ LetBindingItem
                         [ (VarPattern "x", ConstantExpr (IntConst 10))
                         , (VarPattern "y", ConstantExpr (IntConst 20))
                         , (VarPattern "z", ConstantExpr (IntConst 30))
                         ]
                       , LetRecBindingItem
                         [ (VarPattern "process",
                            Lambda [VarPattern "n"]
                              (IfExpr (Application (Identifier "==")
                                       [Identifier "n", ConstantExpr (IntConst 0)])
                                (ConstantExpr (IntConst 0))
                                (Just (Application (Identifier "+")
                                        [ Application (Identifier "process")
                                            [Application (Identifier "-")
                                              [Identifier "n", ConstantExpr (IntConst 1)]]
                                        , Identifier "n" ])) ))
                         ]
                       , EvalExpr (Application (Identifier "process")
                                   [ConstantExpr (IntConst 5)])
                       ]
        parseSuccess parseProgram input expected

  -------------------------------------------------------------------------------
  -- QuickCheck Properties
  -------------------------------------------------------------------------------
  describe "QuickCheck Round-trip Property" $ do
    it "should round-trip via prettyPrint and parse" $
      quickCheck prop_parseProgram
