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