-- | Tests for the megaparsec-based parser (phase 2b).
--
-- Pin Parser2's behaviour on inputs the legacy parser struggles
-- with, especially multi-line bodies and `case`/`let`/lists/cons.
module GDFunc.Parser2Spec (spec) where

import Test.Hspec
import qualified GDFunc.Scanner as Sc
import qualified GDFunc.Parser  as P
import qualified GDFunc.Parser2 as P2

-- | Parse a snippet, expectationFailure on parse failure.
mustParse :: String -> IO P.Module
mustParse src = case P2.parseModule "<inline>" src of
    Right m -> pure m
    Left e  -> error $ "Parser2 should have succeeded:\n" ++ e

spec :: Spec
spec = describe "GDFunc.Parser2 (megaparsec)" $ do

    describe "module headers" $ do
        it "parses a minimal module" $
            case P2.parseModule "<inline>" "module Empty exposing (foo)\n" of
                Right (P.Module ["Empty"] (Just ["foo"]) [] []) -> pure ()
                other -> expectationFailure $ "unexpected: " ++ show other

        it "parses dotted module names" $
            case P2.parseModule "<inline>" "module A.B.C exposing (x)\n" of
                Right (P.Module ["A","B","C"] _ _ _) -> pure ()
                other -> expectationFailure $ "unexpected: " ++ show other

        it "parses multiple items in exposing" $
            case P2.parseModule "<inline>" "module M exposing (a, b, c)\n" of
                Right (P.Module ["M"] (Just ["a","b","c"]) _ _) -> pure ()
                other -> expectationFailure $ "unexpected: " ++ show other

    describe "type annotations" $
        it "parses 'f : Int -> Int'" $
            case P2.parseModule "<inline>" "module M exposing (f)\nf : Int -> Int\n" of
                Right (P.Module _ _ _ [P.TypeAnnotation "f" (P.TArrow (P.TCon "Int" []) (P.TCon "Int" []))]) -> pure ()
                other -> expectationFailure $ "unexpected: " ++ show other

    describe "factorial.gdfunc" $ do
        let src = unlines
                [ "module Factorial exposing (main)"
                , "factorial : Int -> Int"
                , "factorial n ="
                , "    if n <= 1 then"
                , "        1"
                , "    else"
                , "        n * factorial (n - 1)"
                , "main : Int"
                , "main = factorial 5"
                ]

        it "parses end-to-end" $ do
            P.Module n _ _ decls <- mustParse src
            n           `shouldBe` ["Factorial"]
            length decls `shouldBe` 4

        it "Parser2 succeeds where the legacy parser fails on this input" $
            case (legacyParse src, P2.parseModule "<inline>" src) of
                (Right _, _)         -> pendingWith "legacy parser handles this now"
                (Left _, Right _)    -> pure ()
                (Left _, Left e)     -> expectationFailure $
                    "Parser2 should handle this:\n" ++ e

    describe "list literals" $ do
        it "parses an empty list expression" $ do
            P.Module _ _ _ [P.FunctionDecl _ _ body] <-
                mustParse "module M exposing (f)\nf = []\n"
            body `shouldBe` P.EList []

        it "parses a list of integers" $ do
            P.Module _ _ _ [P.FunctionDecl _ _ body] <-
                mustParse "module M exposing (f)\nf = [1, 2, 3]\n"
            body `shouldBe` P.EList [P.EInt 1, P.EInt 2, P.EInt 3]

    describe "case expressions" $ do
        it "parses a two-arm case with cons + list patterns" $ do
            P.Module _ _ _ [P.FunctionDecl _ _ body] <- mustParse $ unlines
                [ "module M exposing (f)"
                , "f list ="
                , "    case list of"
                , "        [] -> 0"
                , "        x :: xs -> x"
                ]
            case body of
                P.ECase (P.EVar "list")
                    [ (P.PList [], P.EInt 0)
                    , (P.PCons (P.PVar "x") (P.PVar "xs"), P.EVar "x")
                    ] -> pure ()
                other -> expectationFailure $ "unexpected case: " ++ show other

        it "parses case with body on its own line (no blank line between arms)" $ do
            P.Module _ _ _ [P.FunctionDecl _ _ body] <- mustParse $ unlines
                [ "module M exposing (f)"
                , "f list ="
                , "    case list of"
                , "        [] ->"
                , "            []"
                , "        pivot :: rest ->"
                , "            42"
                ]
            case body of
                P.ECase _ arms -> length arms `shouldBe` 2
                other -> expectationFailure $ "unexpected: " ++ show other

        it "parses case with blank line between arms" $ do
            P.Module _ _ _ [P.FunctionDecl _ _ body] <- mustParse $ unlines
                [ "module M exposing (f)"
                , "f list ="
                , "    case list of"
                , "        [] ->"
                , "            []"
                , ""
                , "        pivot :: rest ->"
                , "            42"
                ]
            case body of
                P.ECase _ arms -> length arms `shouldBe` 2
                other -> expectationFailure $ "unexpected: " ++ show other

        it "parses case where an arm body would collide with the next arm pattern" $ do
            -- The tricky 'x :: xs -> ...' followed by 'y :: ys -> ...' case
            -- that would confuse a naive expression parser. Layout enforces
            -- that the body for arm 1 doesn't gobble arm 2's pattern.
            P.Module _ _ _ [P.FunctionDecl _ _ body] <- mustParse $ unlines
                [ "module M exposing (f)"
                , "f list ="
                , "    case list of"
                , "        x :: xs -> x"
                , "        y :: ys -> y"
                ]
            case body of
                P.ECase _ arms -> length arms `shouldBe` 2
                other -> expectationFailure $ "unexpected: " ++ show other

    describe "let expressions" $ do
        it "parses a simple single-binding let" $ do
            P.Module _ _ _ [P.FunctionDecl _ _ body] <- mustParse $ unlines
                [ "module M exposing (f)"
                , "f ="
                , "    let x = 1"
                , "    in x"
                ]
            case body of
                P.ELet [P.LetDef "x" [] (P.EInt 1)] (P.EVar "x") -> pure ()
                other -> expectationFailure $ "unexpected: " ++ show other

        it "parses a multi-binding let" $ do
            P.Module _ _ _ [P.FunctionDecl _ _ body] <- mustParse $ unlines
                [ "module M exposing (f)"
                , "f ="
                , "    let"
                , "        a = 1"
                , "        b = 2"
                , "    in a"
                ]
            case body of
                P.ELet bs _ -> length bs `shouldBe` 2
                other -> expectationFailure $ "unexpected: " ++ show other

    describe "example files" $ do
        it "parses borrowing_example.gdfunc end-to-end" $ do
            src <- readFile "examples/borrowing_example.gdfunc"
            case P2.parseModule "examples/borrowing_example.gdfunc" src of
                Right (P.Module ["BorrowingExample"] _ _ decls) ->
                    length decls `shouldSatisfy` (>= 4)
                Right other -> expectationFailure $ "unexpected: " ++ show other
                Left err    -> expectationFailure $ "parse failed:\n" ++ err

        it "parses quicksort_linear.gdfunc end-to-end" $ do
            src <- readFile "examples/quicksort_linear.gdfunc"
            case P2.parseModule "examples/quicksort_linear.gdfunc" src of
                Right (P.Module ["LinearQuicksort"] _ _ decls) ->
                    length decls `shouldSatisfy` (>= 5)
                Right other -> expectationFailure $ "unexpected: " ++ show other
                Left err    -> expectationFailure $ "parse failed:\n" ++ err

  where
    -- Rerun the legacy parser the way the legacy CLI did.
    legacyParse src = do
        toks <- either (Left . show) Right (Sc.scanTokens src)
        let filtered = filter (\t -> case Sc.tokenType t of
                                        Sc.COMMENT _ -> False
                                        _            -> True)
                              toks
        case P.parseModule filtered of
            Right m -> Right m
            Left e  -> Left (show e)
