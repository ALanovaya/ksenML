{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Parser (parseProgram)
import AST
import Data.Either (isRight)

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

  describe "QuickCheck Properties" $ do
    it "parses integer constants correctly" $
      property $ \n ->
        let nonNeg = abs n  -- ensure non-negative numbers for simplicity
        in parseProgram (show nonNeg)
           === Right [EvalExpr (ConstantExpr (IntConst nonNeg))]
    
    it "ensures the factorial function AST has the correct structure" $
      property $
        case parseProgram "let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact" of
          Right [EvalExpr (LetRecBinding bs (Identifier "fact"))] ->
            case bs of
              [(VarPattern "fact", Lambda [VarPattern "n"] _)] -> True
              _ -> False
          _ -> False
