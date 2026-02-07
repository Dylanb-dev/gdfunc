module GDFunc.ScannerSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import GDFunc.Scanner

spec :: Spec
spec = do
    describe "Scanner" $ do
        describe "scanTokens" $ do
            it "scans simple identifiers" $ do
                let result = scanTokens "foo bar baz"
                result `shouldSatisfy` \case
                    Right tokens -> length tokens == 4  -- 3 identifiers + EOF
                    Left _ -> False
            
            it "scans linear identifiers" $ do
                let result = scanTokens "list! handle! builder!"
                result `shouldSatisfy` \case
                    Right tokens -> length tokens == 4
                    Left _ -> False
            
            it "scans numbers" $ do
                let result = scanTokens "42 3.14"
                result `shouldSatisfy` \case
                    Right tokens -> length tokens == 3
                    Left _ -> False
            
            it "scans operators" $ do
                let result = scanTokens "+ - * / -> -o"
                result `shouldSatisfy` \case
                    Right tokens -> length tokens == 7
                    Left _ -> False
            
            it "scans strings" $ do
                let result = scanTokens "\"hello world\""
                result `shouldSatisfy` \case
                    Right [Token (STRING s) _ _, Token EOF _ _] -> s == "hello world"
                    _ -> False
            
            it "scans case!" $ do
                let result = scanTokens "case!"
                result `shouldSatisfy` \case
                    Right [Token CASE_LINEAR _ _, Token EOF _ _] -> True
                    _ -> False
            
            it "scans case without !" $ do
                let result = scanTokens "case"
                result `shouldSatisfy` \case
                    Right [Token CASE _ _, Token EOF _ _] -> True
                    _ -> False
            
            it "distinguishes case from case!" $ do
                let result = scanTokens "case case!"
                result `shouldSatisfy` \case
                    Right [Token CASE _ _, Token CASE_LINEAR _ _, Token EOF _ _] -> True
                    _ -> False