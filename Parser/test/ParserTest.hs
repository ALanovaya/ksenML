{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
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

main :: IO ()
main = hspec $ do
  describe "Parser Module" $ do

    describe "parseConstant" $ do
      it "parses an integer constant" $ do
        parseSuccess parseConstant "42" (IntConst 42)
      
      it "parses a boolean constant (true)" $ do
        parseSuccess parseConstant "true" (BoolConst True)
      
      it "parses a boolean constant (false)" $ do
        parseSuccess parseConstant "false" (BoolConst False)
    
    describe "identifier" $ do
      it "parses a valid identifier" $ do
        parseSuccess identifier "varName" "varName"
      
      it "rejects reserved words" $ do
        parseFailure identifier "if"
    
    describe "parseType" $ do
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
    
    describe "parsePattern" $ do
      it "parses a wildcard pattern" $ do
        parseSuccess parsePattern "_" Wildcard
      
      it "parses a variable pattern" $ do
        parseSuccess parsePattern "x" (VarPattern "x")
      
      it "parses a constant pattern" $ do
        parseSuccess parsePattern "42" (ConstPattern (IntConst 42))
      
      it "parses a pattern with type annotation" $ do
        parseSuccess parsePattern "x : int" (TypePattern (VarPattern "x") TypeInt)
    
    describe "parseLambdaExpr" $ do
      it "parses a simple lambda expression" $ do
        let input = "fun x -> x"
            expected = Lambda [VarPattern "x"] (Identifier "x")
        parseSuccess parseLambdaExpr input expected
    
    describe "parseLetExpr" $ do
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
    
    describe "parseIfExpr" $ do
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
    
    describe "parseMatchExpr" $ do
      it "parses a simple match expression" $ do
        let input = "match x with | _ -> 1"
            expected = MatchExpr (Identifier "x") [(Wildcard, ConstantExpr (IntConst 1))]
        parseSuccess parseMatchExpr input expected
    
    describe "parseExpr (function application and binary operators)" $ do
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
    
    describe "parseTopLevelItem" $ do
      it "parses a top-level let binding" $ do
        let input = "let x = 1"
            expected = LetBindingItem [(VarPattern "x", ConstantExpr (IntConst 1))]
        parseSuccess parseTopLevelItem input expected
      
      it "parses an expression for evaluation" $ do
        let input = "42"
            expected = EvalExpr (ConstantExpr (IntConst 42))
        parseSuccess parseTopLevelItem input expected
    
    describe "parseProgram" $ do
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

      it "parses nested let expressions" $ do
        let input = "let x = 1 in let y = 2 in x + y"
            expected = LetBinding [(VarPattern "x", ConstantExpr (IntConst 1))]
                        (LetBinding [(VarPattern "y", ConstantExpr (IntConst 2))]
                          (Application (Identifier "+")
                            [ Identifier "x"
                            , Identifier "y"
                            ]))
        parseSuccess parseLetExpr input expected

      it "fails to parse a let expression missing a proper binding" $ do
        parseFailure parseLetExpr "let x = in x"

      it "parses match expression with multiple cases" $ do
        let input = "match x with | 0 -> 1 | _ -> 2"
            expected = MatchExpr (Identifier "x")
                        [ (ConstPattern (IntConst 0), ConstantExpr (IntConst 1))
                        , (Wildcard, ConstantExpr (IntConst 2))
                        ]
        parseSuccess parseMatchExpr input expected

      it "parses nested if expressions" $ do
        let input = "if true then if false then 1 else 2 else 3"
            expected = IfExpr (ConstantExpr (BoolConst True))
                        (IfExpr (ConstantExpr (BoolConst False))
                          (ConstantExpr (IntConst 1))
                          (Just (ConstantExpr (IntConst 2))))
                        (Just (ConstantExpr (IntConst 3)))
        parseSuccess parseIfExpr input expected

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
                                , ConstantExpr (IntConst 44)])
                      ]
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
                                ])
                      ]
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
                                ])
                      ]
        parseSuccess parseProgram input expected

      it "parses nested lambda with let expression inside" $ do
        let input = "fun a b -> let c = a + b in c * c"
            expected = Lambda [VarPattern "a", VarPattern "b"]
                        (LetBinding [(VarPattern "c", Application (Identifier "+") [Identifier "a", Identifier "b"])]
                          (Application (Identifier "*") [Identifier "c", Identifier "c"]))
        parseSuccess parseLambdaExpr input expected

      it "parses if expression with complex binary conditions" $ do
        let input = "if a * b == c then a + b else a - b"
            expected = IfExpr (Application (Identifier "==")
                                      [ Application (Identifier "*") [Identifier "a", Identifier "b"]
                                      , Identifier "c" ])
                              (Application (Identifier "+") [Identifier "a", Identifier "b"])
                              (Just (Application (Identifier "-") [Identifier "a", Identifier "b"]))
        parseSuccess parseIfExpr input expected

      it "parses parenthesized type annotation on binary expression" $ do
        let input = "(1 + 2) : int"
            expected = TypeAnnotation (Application (Identifier "+")
                                               [ConstantExpr (IntConst 1), ConstantExpr (IntConst 2)])
                                      TypeInt
        parseSuccess parseExpr input expected

      it "parses lambda with multiple annotated parameters" $ do
        let input = "fun (x : int) (y : bool) -> if y then x else x + 1"
            expected = Lambda [ TypePattern (VarPattern "x") TypeInt
                              , TypePattern (VarPattern "y") TypeBool ]
                         (IfExpr (Identifier "y")
                           (Identifier "x")
                           (Just (Application (Identifier "+")
                                  [Identifier "x", ConstantExpr (IntConst 1)])))
        parseSuccess parseLambdaExpr input expected

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

      it "parses a program with factorial and fibonacci functions" $ do
        let input = unlines
              [ "let rec fact = fun n ->"
              , "  if n == 0 then 1 else n * fact (n - 1)"
              , "let rec fib = fun n ->"
              , "  match n with"
              , "  | 0 -> 0"
              , "  | 1 -> 1"
              , "  | _ -> fib (n - 1) + fib (n - 2)"
              , "fact 5 + fib 7"
              ]
            expected = [ LetRecBindingItem
                         [ (VarPattern "fact",
                            Lambda [VarPattern "n"]
                              (IfExpr (Application (Identifier "==")
                                      [Identifier "n", ConstantExpr (IntConst 0)])
                                (ConstantExpr (IntConst 1))
                                (Just (Application (Identifier "*")
                                        [Identifier "n",
                                         Application (Identifier "fact")
                                           [Application (Identifier "-")
                                             [Identifier "n", ConstantExpr (IntConst 1)]]
                                        ]))))
                         ]
                       , LetRecBindingItem
                         [ (VarPattern "fib",
                            Lambda [VarPattern "n"]
                              (MatchExpr (Identifier "n")
                                [ (ConstPattern (IntConst 0), ConstantExpr (IntConst 0))
                                , (ConstPattern (IntConst 1), ConstantExpr (IntConst 1))
                                , (Wildcard,
                                   Application (Identifier "+")
                                     [ Application (Identifier "fib")
                                         [Application (Identifier "-")
                                           [Identifier "n", ConstantExpr (IntConst 1)]]
                                     , Application (Identifier "fib")
                                         [Application (Identifier "-")
                                           [Identifier "n", ConstantExpr (IntConst 2)]]
                                     ])
                                ]))
                         ]
                       , EvalExpr (Application (Identifier "+")
                                    [ Application (Identifier "fact") [ConstantExpr (IntConst 5)]
                                    , Application (Identifier "fib") [ConstantExpr (IntConst 7)]
                                    ])
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
                                        , Identifier "n" ]))))
                         ]
                       , EvalExpr (Application (Identifier "process")
                                   [ConstantExpr (IntConst 5)])
                       ]
        parseSuccess parseProgram input expected