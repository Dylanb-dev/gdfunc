module GDFunc.ScannerSpec (spec) where

import Test.Hspec
import GDFunc.Scanner

spec :: Spec
spec = do
    describe "Scanner" $ do
        describe "scanTokens" $ do
            it "scans simple identifiers" $ do
                let result = scanTokens "foo bar baz list handle"
                result `shouldSatisfy` \case
                    Right tokens -> length tokens == 6 -- 5 idents + EOF
                    Left _ -> False

            it "scans ampersand for borrowing" $ do
                let result = scanTokens "& && &list"
                result `shouldSatisfy` \case
                    Right [ Token AMPERSAND _ _
                          , Token AND _ _
                          , Token AMPERSAND _ _
                          , Token (IDENTIFIER "list") _ _
                          , Token EOF _ _
                          ] -> True
                    _ -> False

            it "scans shared keyword" $ do
                let result = scanTokens "shared Int"
                result `shouldSatisfy` \case
                    Right [ Token SHARED _ _
                          , Token (IDENTIFIER "Int") _ _
                          , Token EOF _ _
                          ] -> True
                    _ -> False
            
            it "scans numbers (integers and floats)" $ do
                let result = scanTokens "42 3.14 5 1"
                result `shouldSatisfy` \case
                    Right tokens -> length tokens == 5
                    Left _ -> False
            
            it "scans operators and punctuation" $ do
                -- Combined existing ops with Factorial symbols: : <= = ( )
                let result = scanTokens "+ - * / -> -o : <= = ( )"
                result `shouldSatisfy` \case
                    Right tokens -> length tokens == 12
                    Left _ -> False
            
            it "scans strings" $ do
                let result = scanTokens "\"hello world\""
                result `shouldSatisfy` \case
                    Right [Token (STRING s) _ _, Token EOF _ _] -> s == "hello world"
                    _ -> False
            
            it "handles keywords and case variants" $ do
                let result = scanTokens "case case! if then else module exposing"
                result `shouldSatisfy` \case
                    Right [ Token CASE _ _
                          , Token CASE_LINEAR _ _
                          , Token IF _ _
                          , Token THEN _ _
                          , Token ELSE _ _
                          , Token MODULE _ _
                          , Token EXPOSING _ _
                          , Token EOF _ _
                          ] -> True
                    _ -> False

            it "scans the Factorial module definition correctly" $ do
                -- A focused test for the specific line: factorial : Int -> Int
                let result = scanTokens "factorial : Int -> Int"
                result `shouldSatisfy` \case
                    Right [ Token (IDENTIFIER "factorial") _ _
                          , Token COLON _ _
                          , Token (IDENTIFIER "Int") _ _
                          , Token ARROW _ _
                          , Token (IDENTIFIER "Int") _ _
                          , Token EOF _ _
                          ] -> True
                    _ -> False

            it "distinguishes case from case!" $ do
                let result = scanTokens "case case!"
                result `shouldSatisfy` \case
                    Right [Token CASE _ _, Token CASE_LINEAR _ _, Token EOF _ _] -> True
                    _ -> False

        describe "Example Files" $ do
            it "scans factorial.gdfunc" $ do
                let source = unlines
                        [ "module Factorial exposing (main)"
                        , ""
                        , "factorial : Int -> Int"
                        , "factorial n ="
                        , "    if n <= 1 then"
                        , "        1"
                        , "    else"
                        , "        n * factorial (n - 1)"
                        , ""
                        , "main : Int"
                        , "main = factorial 5"
                        ]
                let result = scanTokens source
                result `shouldSatisfy` \case
                    Right tokens -> length tokens > 30  -- Should have many tokens
                    Left _ -> False

            it "scans borrowing_example.gdfunc" $ do
                let source = unlines
                        [ "module BorrowingExample exposing (main)"
                        , ""
                        , "length : &List a -> Int"
                        , "length list ="
                        , "    case list of"
                        , "        [] -> 0"
                        , "        _ :: rest -> 1 + length &rest"
                        , ""
                        , "main : Int"
                        , "main ="
                        , "    let numbers = [1, 2, 3]"
                        , "        len = length &numbers"
                        , "        total = sum numbers"
                        , "    in len + total"
                        ]
                let result = scanTokens source
                result `shouldSatisfy` \case
                    Right tokens -> length tokens > 40  -- Should have many tokens
                    Left _ -> False

            it "scans shared_types.gdfunc" $ do
                let source = unlines
                        [ "module SharedTypesExample exposing (main)"
                        , ""
                        , "type shared Config = Config"
                        , "    { host : String"
                        , "    , port : Int"
                        , "    }"
                        , ""
                        , "type shared Point = Point Float Float"
                        ]
                let result = scanTokens source
                result `shouldSatisfy` \case
                    Right tokens ->
                        -- Check for presence of SHARED keyword
                        any (\t -> case tokenType t of
                            SHARED -> True
                            _ -> False) tokens
                    Left _ -> False

            it "scans linear_default.gdfunc" $ do
                let source = unlines
                        [ "module LinearDefaultExample exposing (main)"
                        , ""
                        , "type Stack a = Stack (List a)"
                        , ""
                        , "isEmpty : &Stack a -> Bool"
                        , "isEmpty stack ="
                        , "    case stack of"
                        , "        Stack [] -> True"
                        , "        Stack _ -> False"
                        ]
                let result = scanTokens source
                result `shouldSatisfy` \case
                    Right tokens ->
                        -- Check for presence of AMPERSAND for borrowing
                        any (\t -> case tokenType t of
                            AMPERSAND -> True
                            _ -> False) tokens
                    Left _ -> False

            it "scans quicksort_linear.gdfunc" $ do
                let source = unlines
                        [ "module LinearQuicksort exposing (quicksort)"
                        , ""
                        , "quicksort : List Int -> List Int"
                        , "quicksort list ="
                        , "    case list of"
                        , "        [] -> []"
                        , "        pivot :: rest ->"
                        , "            let (smaller, larger) = partition pivot rest"
                        , "            in concat3 (quicksort smaller) pivot (quicksort larger)"
                        ]
                let result = scanTokens source
                result `shouldSatisfy` \case
                    Right tokens -> length tokens > 35  -- Should have many tokens
                    Left _ -> False