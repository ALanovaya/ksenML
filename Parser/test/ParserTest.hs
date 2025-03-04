{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Parser (parseProgram)
import AST

main :: IO ()
main = hspec $ do
  describe "Parser Tests" $ do
    it "parses integer literal" $ do
      parseProgram "42" `shouldBe` Right [EvalExpr (ConstantExpr (IntConst 42))]

    it "parses let-expression" $ do
      let expected = [EvalExpr (LetBinding [(VarPattern "x", ConstantExpr (IntConst 42))] (Identifier "x"))]
      parseProgram "let x = 42 in x" `shouldBe` Right expected

    it "parses lambda-expression" $ do
      let expected = [EvalExpr (Lambda [VarPattern "x"] (Identifier "x"))]
      parseProgram "fun x -> x" `shouldBe` Right expected

    it "parses function application" $ do
      let expected = [EvalExpr (Application (Identifier "f") (ConstantExpr (IntConst 42)))]
      parseProgram "f 42" `shouldBe` Right expected

    it "parses if-expression" $ do
      let expected = [EvalExpr (IfExpr (Identifier "true")
                                       (ConstantExpr (IntConst 1))
                                       (Just (ConstantExpr (IntConst 0))))]
      parseProgram "if true then 1 else 0" `shouldBe` Right expected

    it "parses match-expression" $ do
      let expected = [EvalExpr (MatchExpr (Identifier "x")
                                   [(ConstPattern (IntConst 1), ConstantExpr (IntConst 2))])]
      parseProgram "match x with | 1 -> 2" `shouldBe` Right expected

    it "parses type definition" $ do
      let expected = [TypeDef (TypeDeclaration "List" ["a"]
                         [ ConstructorDecl "Cons" (Just (TypeVar "a"))
                         , ConstructorDecl "Nil" Nothing])]
      parseProgram "type 'a List = Cons of 'a | Nil" `shouldBe` Right expected

    it "parses list literal as sequential expressions" $ do
      let cons e1 e2 = Application (Application (Identifier "::") e1) e2
          expected = [EvalExpr (cons (ConstantExpr (IntConst 1))
                              (cons (ConstantExpr (IntConst 2))
                                    (ConstructorExpr "[]" Nothing)))]
      parseProgram "[1; 2]" `shouldBe` Right expected

    it "parses top-level let binding" $ do
      let expected = [LetBindingItem [(VarPattern "x", ConstantExpr (IntConst 42))]]
      parseProgram "let x = 42" `shouldBe` Right expected

    it "parses complex nested expressions" $ do
      let input = "let f = fun x -> if x then let y = 42 in y else 0 in f true"
          expected =
            [ EvalExpr
                (LetBinding
                  [ ( VarPattern "f"
                    , Lambda [VarPattern "x"]
                        (IfExpr (Identifier "x")
                          (LetBinding [(VarPattern "y", ConstantExpr (IntConst 42))] (Identifier "y"))
                          (Just (ConstantExpr (IntConst 0)))
                        )
                    )
                  ]
                  (Application (Identifier "f") (Identifier "true"))
                )
            ]
      parseProgram input `shouldBe` Right expected

    it "parses a simple let rec binding" $ do
      let input = "let rec id x = x in id 42"
          expected =
            EvalExpr (LetRecBinding
                      [(VarPattern "id", Lambda [VarPattern "x"] (Identifier "x"))]
                      (Application (Identifier "id") (ConstantExpr (IntConst 42))))
      parseProgram input `shouldBe` Right [expected]

    it "parses factorial function" $ do
      let input = "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact"
          expected =
            EvalExpr $
              LetRecBinding
                [ ( VarPattern "fact",
                    Lambda [VarPattern "n"]
                      (IfExpr
                        (Application (Application (Identifier "=") (Identifier "n"))
                                     (ConstantExpr (IntConst 0)))
                        (ConstantExpr (IntConst 1))
                        (Just (Application
                                (Application (Identifier "*") (Identifier "n"))
                                (Application (Identifier "fact")
                                  (Application (Application (Identifier "-") (Identifier "n"))
                                               (ConstantExpr (IntConst 1)))))))
                  )
                ]
                (Identifier "fact")
      parseProgram input `shouldBe` Right [expected]

    it "parses nested parentheses correctly" $ do
      let input = "(((42)))"
          expected = [EvalExpr (ConstantExpr (IntConst 42))]
      parseProgram input `shouldBe` Right expected

    it "parses expressions with multiple binary operators respecting precedence" $ do
      let input = "1 + 2 * 3 - 4"
          -- Ожидаемый разбор (с учётом левоассоциативности для + и - и приоритета * выше)
          expected = [EvalExpr (
                        Application
                          (Application (Identifier "-")
                            (Application
                              (Application (Identifier "+") (ConstantExpr (IntConst 1)))
                              (Application (Application (Identifier "*") (ConstantExpr (IntConst 2)))
                                           (ConstantExpr (IntConst 3))))
                          )
                          (ConstantExpr (IntConst 4))
                      )]
      parseProgram input `shouldBe` Right expected

    it "parses expressions with type annotations" $ do
      let input = "42 : int"
          expected = [EvalExpr (TypeAnnotation (ConstantExpr (IntConst 42)) (TypeConstructor "int" []))]
      parseProgram input `shouldBe` Right expected

    it "parses tuple expressions" $ do
      let input = "(1, 2)"
          expected = [EvalExpr (TupleExpr [ConstantExpr (IntConst 1), ConstantExpr (IntConst 2)])]
      parseProgram input `shouldBe` Right expected

    it "parses empty list literal" $ do
      let input = "[]"
          expected = [EvalExpr (ConstructorExpr "[]" Nothing)]
      parseProgram input `shouldBe` Right expected

    it "parses a list with multiple elements" $ do
      let cons e1 e2 = Application (Application (Identifier "::") e1) e2
          expected = [EvalExpr (cons (ConstantExpr (IntConst 1))
                              (cons (ConstantExpr (IntConst 2))
                                    (cons (ConstantExpr (IntConst 3))
                                          (ConstructorExpr "[]" Nothing))))]
      parseProgram "[1; 2; 3]" `shouldBe` Right expected

    it "parses let binding with multiple bindings using 'and'" $ do
      let input = "let x = 1 and y = 2 in x + y"
          expected = [EvalExpr (LetBinding [(VarPattern "x", ConstantExpr (IntConst 1)), (VarPattern "y", ConstantExpr (IntConst 2))]
                                       (Application (Application (Identifier "+") (Identifier "x")) (Identifier "y")))]
      parseProgram input `shouldBe` Right expected

    it "parses a lambda with multiple parameters" $ do
      let input = "fun x y z -> x"
          expected = [EvalExpr (Lambda [VarPattern "x", VarPattern "y", VarPattern "z"] (Identifier "x"))]
      parseProgram input `shouldBe` Right expected

    it "parses match expression with constant patterns" $ do
      let input = "match 2 with | 0 -> \"zero\" | 1 -> \"one\" | 2 -> \"two\" | _ -> \"other\""
          expected =
            [ EvalExpr (MatchExpr
                (ConstantExpr (IntConst 2))
                [ (ConstPattern (IntConst 0), ConstantExpr (StringConst "zero"))
                , (ConstPattern (IntConst 1), ConstantExpr (StringConst "one"))
                , (ConstPattern (IntConst 2), ConstantExpr (StringConst "two"))
                , (Wildcard,           ConstantExpr (StringConst "other"))
                ]
              )
            ]
      parseProgram input `shouldBe` Right expected

    it "parses match expression with tuple pattern" $ do
      let input = "match (3, 4) with (x, y) -> x + y"
          expected =
            [ EvalExpr (MatchExpr
                (TupleExpr [ConstantExpr (IntConst 3), ConstantExpr (IntConst 4)])
                [ ( TuplePattern [VarPattern "x", VarPattern "y"]
                  , Application
                      (Application (Identifier "+") (Identifier "x"))
                      (Identifier "y")
                  )
                ]
              )
            ]
      parseProgram input `shouldBe` Right expected

    it "parses match expression with nested tuple pattern" $ do
      let input = "match ((1, 2), 3) with ((x, y), z) -> x + y + z"
          expected =
            [ EvalExpr (MatchExpr
                (TupleExpr [ TupleExpr [ConstantExpr (IntConst 1), ConstantExpr (IntConst 2)]
                           , ConstantExpr (IntConst 3)
                           ])
                [ ( TuplePattern [ TuplePattern [VarPattern "x", VarPattern "y"]
                                , VarPattern "z"
                                ]
                  , Application
                      (Application (Identifier "+")
                        (Application
                          (Application (Identifier "+") (Identifier "x"))
                          (Identifier "y")
                        )
                      )
                      (Identifier "z")
                  )
                ]
              )
            ]
      parseProgram input `shouldBe` Right expected

    it "parses match expression with list pattern" $ do
      let input = "match [1; 2; 3] with | [] -> [] | x :: xs -> [x]"
          nilPat = ConstructPattern "[]" Nothing
          consPat p acc = ConstructPattern "::" (Just (TuplePattern [p, acc]))
          listExpr = foldr (\n acc -> Application (Application (Identifier "::") (ConstantExpr (IntConst n))) acc)
                           (ConstructorExpr "[]" Nothing)
                           [1, 2, 3]
          rhsExpr = Application (Application (Identifier "::") (Identifier "x"))
                                (ConstructorExpr "[]" Nothing)
          expected =
            [ EvalExpr (MatchExpr
                listExpr
                [ ( nilPat
                  , ConstructorExpr "[]" Nothing
                  )
                , ( consPat (VarPattern "x") (VarPattern "xs")
                  , rhsExpr
                  )
                ]
              )
            ]
      parseProgram input `shouldBe` Right expected

    it "parses match expression with tuple and list pattern" $ do
      let input = "match (3, [1; 2; 3]) with | (x, []) -> x | (x, y) -> y"
          nilPat = ConstructPattern "[]" Nothing
          exprList = foldr (\n acc -> Application (Application (Identifier "::") (ConstantExpr (IntConst n))) acc)
                           (ConstructorExpr "[]" Nothing)
                           [1, 2, 3]
          expected =
            [ EvalExpr (MatchExpr
                (TupleExpr [ConstantExpr (IntConst 3), exprList])
                [ ( TuplePattern [VarPattern "x", nilPat]
                  , Identifier "x"
                  )
                , ( TuplePattern [VarPattern "x", VarPattern "y"]
                  , Identifier "y"
                  )
                ]
              )
            ]
      parseProgram input `shouldBe` Right expected

    it "parses match expression with multiple list patterns" $ do
      let input = unlines
            [ "match (5, [1; 2; 3]) with"
            , "| (x, []) -> \"empty list\""
            , "| (x, [y]) -> \"one element in list\""
            , "| (x, y :: ys) -> \"head\""
            ]
          nilPat = ConstructPattern "[]" Nothing
          consPat p acc = ConstructPattern "::" (Just (TuplePattern [p, acc]))
          exprList = foldr (\n acc -> Application (Application (Identifier "::") (ConstantExpr (IntConst n))) acc)
                           (ConstructorExpr "[]" Nothing)
                           [1, 2, 3]
          expected =
            [ EvalExpr (MatchExpr
                (TupleExpr [ConstantExpr (IntConst 5), exprList])
                [ ( TuplePattern [VarPattern "x", nilPat]
                  , ConstantExpr (StringConst "empty list")
                  )
                , ( TuplePattern [VarPattern "x", consPat (VarPattern "y") nilPat]
                  , ConstantExpr (StringConst "one element in list")
                  )
                , ( TuplePattern [VarPattern "x", consPat (VarPattern "y") (VarPattern "ys")]
                  , ConstantExpr (StringConst "head")
                  )
                ]
              )
            ]
      parseProgram input `shouldBe` Right expected

  describe "Lists Expressions" $ do
    
    it "parses empty list" $ do
      let expected = [ EvalExpr (ConstructorExpr "[]" Nothing) ]
      parseProgram "[]" `shouldBe` Right expected

    it "parses list literal with integers" $ do
      let expected = [ EvalExpr 
            ( Application 
                (Application (Identifier "::") (ConstantExpr (IntConst 1)))
                ( Application 
                    (Application (Identifier "::") (ConstantExpr (IntConst 2)))
                    ( Application 
                        (Application (Identifier "::") (ConstantExpr (IntConst 3)))
                        (ConstructorExpr "[]" Nothing)
                    )
                )
            )
            ]
      parseProgram "[1; 2; 3]" `shouldBe` Right expected

    it "parses list literal with identifiers" $ do
      let expected = [ EvalExpr 
            ( Application 
                (Application (Identifier "::") (Identifier "x"))
                ( Application 
                    (Application (Identifier "::") (Identifier "y"))
                    ( Application 
                        (Application (Identifier "::") (Identifier "z"))
                        (ConstructorExpr "[]" Nothing)
                    )
                )
            )
            ]
      parseProgram "[x; y; z]" `shouldBe` Right expected

    it "parses cons chain for list" $ do
      let expected = [ EvalExpr 
            ( Application 
                (Application (Identifier "::") (Identifier "x"))
                ( Application 
                    (Application (Identifier "::") (Identifier "y"))
                    ( Application 
                        (Application (Identifier "::") (Identifier "z"))
                        (ConstructorExpr "[]" Nothing)
                    )
                )
            )
            ]
      parseProgram "x :: y :: z :: []" `shouldBe` Right expected

    it "parses list with tuple elements" $ do
      let tuple a b = TupleExpr [ ConstantExpr (IntConst a)
                                  , ConstantExpr (IntConst b)
                                  ]
          expected = [ EvalExpr 
            ( Application 
                (Application (Identifier "::") (tuple 0 0))
                ( Application 
                    (Application (Identifier "::") (tuple 1 2))
                    ( Application 
                        (Application (Identifier "::") (tuple 3 4))
                        ( Application 
                            (Application (Identifier "::") (tuple 5 6))
                            (ConstructorExpr "[]" Nothing)
                        )
                    )
                )
            )
            ]
      parseProgram "(0, 0) :: (1, 2) :: [(3, 4); (5, 6)]" `shouldBe` Right expected

    it "parses list of functions" $ do
      let fun1 = Lambda [VarPattern "x", VarPattern "y"] (Identifier "x")
          fun2 = Lambda [VarPattern "x", Wildcard] (Identifier "x")
          expected = [ EvalExpr 
            ( Application 
                (Application (Identifier "::") fun1)
                ( Application 
                    (Application (Identifier "::") fun2)
                    (ConstructorExpr "[]" Nothing)
                )
            )
            ]
      parseProgram "(fun x y -> x) :: (fun x _ -> x) :: []" `shouldBe` Right expected

  describe "Tuple Expressions" $ do

    it "parses empty tuple as unit" $ do
      let expected = [ EvalExpr (ConstantExpr (StringConst "unit")) ]
      parseProgram "()" `shouldBe` Right expected

    it "parses tuple of two elements" $ do
      let expected = [ EvalExpr (TupleExpr [ ConstantExpr (IntConst 1)
                                            , ConstantExpr (IntConst 2)
                                            ])
                     ]
      parseProgram "(1, 2)" `shouldBe` Right expected

    it "parses tuple of three elements" $ do
      let expected = [ EvalExpr (TupleExpr [ ConstantExpr (IntConst 1)
                                            , ConstantExpr (IntConst 2)
                                            , ConstantExpr (IntConst 3)
                                            ])
                     ]
      parseProgram "(1, 2, 3)" `shouldBe` Right expected

    it "parses tuple with identifier" $ do
      let expected = [ EvalExpr (TupleExpr [ ConstantExpr (IntConst 1)
                                            , ConstantExpr (IntConst 2)
                                            , Identifier "a"
                                            ])
                     ]
      parseProgram "(1, 2, a)" `shouldBe` Right expected

    it "parses tuple of functions" $ do
      let funx = Lambda [VarPattern "x"] (Identifier "x")
          funy = Lambda [VarPattern "y"] (Identifier "y")
          expected = [ EvalExpr (TupleExpr [ funx, funy ]) ]
      parseProgram "(fun x -> x, fun y -> y)" `shouldBe` Right expected

    it "parses complex tuple" $ do
      let listExpr = 
            Application 
              (Application (Identifier "::") (ConstantExpr (IntConst 1)))
              ( Application 
                  (Application (Identifier "::") (ConstantExpr (IntConst 3)))
                  ( Application 
                      (Application (Identifier "::") (ConstantExpr (IntConst 5)))
                      (ConstructorExpr "[]" Nothing)
                  )
              )
          expected = [ EvalExpr (TupleExpr [ ConstantExpr (IntConst 1)
                                            , Lambda [VarPattern "x"] (Identifier "x")
                                            , ConstantExpr (StringConst "unit")
                                            , listExpr
                                            ])
                     ]
      parseProgram "(1, fun x -> x, (), [1 ; 3; 5])" `shouldBe` Right expected

    it "parses nested tuple" $ do
      let expected = [ EvalExpr (TupleExpr [ TupleExpr [ ConstantExpr (IntConst 1)
                                                       , ConstantExpr (IntConst 2)
                                                       ]
                                            , ConstantExpr (IntConst 1)
                                            ])
                     ]
      parseProgram "((1, 2), 1)" `shouldBe` Right expected

    it "parses tuple with function application" $ do
      let funx = Lambda [VarPattern "x"] (Identifier "x")
          funy = Lambda [VarPattern "y"] (Identifier "y")
          app1 = Application funx (ConstantExpr (IntConst 0))
          app2 = Application funy (ConstantExpr (IntConst 0))
          expected = [ EvalExpr (TupleExpr [ app1, app2 ]) ]
      parseProgram "((fun x -> x) 0, (fun y -> y) 0)" `shouldBe` Right expected

    it "parses tuple with let bindings" $ do
      let let1 = LetBinding [(VarPattern "f", Lambda [VarPattern "x"] (Identifier "x"))]
                  (Application (Identifier "f") (ConstantExpr (IntConst 0)))
          let2 = LetBinding [(VarPattern "f", Lambda [VarPattern "y"] (Identifier "y"))]
                  (ConstantExpr (IntConst 0))
          expected = [ EvalExpr (TupleExpr [ let1, let2 ]) ]
      parseProgram "(let f x = x in f 0, let f y = y in 0)" `shouldBe` Right expected