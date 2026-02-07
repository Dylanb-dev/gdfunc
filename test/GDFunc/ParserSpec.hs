module GDFunc.ParserSpec (spec) where

import Test.Hspec
import GDFunc.Scanner
import GDFunc.Parser

spec :: Spec
spec = do
    describe "Parser" $ do
        describe "parseExpr" $ do
            it "parses simple variables" $ do
                let tokens = either (const []) id $ scanTokens "foo"
                parseExpr tokens `shouldBe` Right (EVar "foo")
            
            it "parses linear variables" $ do
                let tokens = either (const []) id $ scanTokens "list!"
                parseExpr tokens `shouldBe` Right (ELinearVar "list")
            
            it "parses integers" $ do
                let tokens = either (const []) id $ scanTokens "42"
                parseExpr tokens `shouldBe` Right (EInt 42)
            
            it "parses lists" $ do
                let tokens = either (const []) id $ scanTokens "[1, 2, 3]"
                parseExpr tokens `shouldBe` 
                    Right (EList [EInt 1, EInt 2, EInt 3])
            
            it "parses if expressions" $ do
                let tokens = either (const []) id $ 
                        scanTokens "if x then y else z"
                case parseExpr tokens of
                    Right (EIf _ _ _) -> True `shouldBe` True
                    _ -> expectationFailure "Failed to parse if expression"
        
        describe "parseType" $ do
            it "parses simple types" $ do
                let tokens = either (const []) id $ scanTokens "Int"
                parseType tokens `shouldBe` Right (TCon "Int" [])
            
            it "parses linear types" $ do
                let tokens = either (const []) id $ scanTokens "List! Int"
                case parseType tokens of
                    Right (TLinear _) -> True `shouldBe` True
                    _ -> expectationFailure "Failed to parse linear type"
            
            it "parses function types" $ do
                let tokens = either (const []) id $ scanTokens "Int -> String"
                case parseType tokens of
                    Right (TArrow _ _) -> True `shouldBe` True
                    _ -> expectationFailure "Failed to parse function type"
                    
            it "debug: shows tokens for 'List! Int'" $ do
                let result = scanTokens "List! Int"
                case result of
                    Right tokens -> do
                        putStrLn $ "\nTokens for 'List! Int':"
                        mapM_ (putStrLn . ("  " ++) . show) tokens
                        True `shouldBe` True
                    Left err -> expectationFailure $ "Scan error: " ++ err

            it "parses linear function types" $ do
                let tokens = either (const []) id $ scanTokens "a -o b"
                case parseType tokens of
                    Right (TLinearArrow _ _) -> True `shouldBe` True
                    _ -> expectationFailure "Failed to parse linear function type"
