module GDFunc.ParserSpec (spec) where

import Test.Hspec
import GDFunc.Scanner
import GDFunc.Parser
import Data.List (find, isInfixOf)

spec :: Spec
spec = do
    describe "Parser" $ do
        describe "parseExpr" $ do
            it "parses simple variables" $ do
                let tokens = either (const []) id $ scanTokens "foo"
                parseExpr tokens `shouldBe` Right (EVar "foo")
            
            it "parses borrowed variables" $ do
                let tokens = either (const []) id $ scanTokens "&list"
                parseExpr tokens `shouldBe` Right (EBorrow (EVar "list"))
            
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
            
            it "parses borrowed types" $ do
                let tokens = either (const []) id $ scanTokens "&List Int"
                case parseType tokens of
                    Right (TBorrowed _) -> True `shouldBe` True
                    e -> expectationFailure ("Failed to parse borrowed type: " ++ show e)

            it "parses shared types" $ do
                let tokens = either (const []) id $ scanTokens "shared Int"
                case parseType tokens of
                    Right (TShared _) -> True `shouldBe` True
                    e -> expectationFailure ("Failed to parse shared type: " ++ show e)
            
            it "parses function types" $ do
                let tokens = either (const []) id $ scanTokens "Int -> String"
                case parseType tokens of
                    Right (TArrow _ _) -> True `shouldBe` True
                    _ -> expectationFailure "Failed to parse function type"
                    
        --     it "debug: shows tokens for 'List! Int'" $ do
        --         let result = scanTokens "List! Int"
        --         case result of
        --             Right tokens -> do
        --                 putStrLn $ "\nTokens for 'List! Int':"
        --                 mapM_ (putStrLn . ("  " ++) . show) tokens
        --                 True `shouldBe` True
        --             Left err -> expectationFailure $ "Scan error: " ++ err

        --     it "parses linear function types" $ do
        --         let tokens = either (const []) id $ scanTokens "a -o b"
        --         case parseType tokens of
        --             Right (TLinearArrow _ _) -> True `shouldBe` True
        --             _ -> expectationFailure "Failed to parse linear function type"

        --     it "parses complex nested function types" $ do
        --         let tokens = either (const []) id $ scanTokens "Int -> Int -> Int"
        --         case parseType tokens of
        --             Right (TArrow _ (TArrow _ _)) -> True `shouldBe` True
        --             _ -> expectationFailure "Failed to parse right-associative function type"
        
        -- describe "parseModule" $ do
            -- it "parses a module with ML type annotations and function bodies" $ do
            --     let source = unlines
            --             [ "module Factorial exposing (main)"
            --             , ""
            --             , "factorial : Int -> Int"
            --             , "factorial n = 1"
            --             ]
            --     let tokens = either (const []) id $ scanTokens source
            --     let filteredTokens = filter (not . isSkippable . tokenType) tokens
            --     case parseModule filteredTokens of
            --         Right (Module _ _ _ decls) -> do
            --             -- Factorial should now result in TWO declarations
            --             length decls `shouldBe` 2
                        
            --             -- Check for the Annotation
            --             any (\case 
            --                 TypeAnnotation "factorial" _ -> True
            --                 _ -> False) decls `shouldBe` True
                        
            --             -- Check for the Function Body
            --             any (\case 
            --                 FunctionDecl "factorial" _ _ -> True
            --                 _ -> False) decls `shouldBe` True
                            
            --         Left err -> expectationFailure $ "Parse error: " ++ show err

            -- it "parses full factorial logic with recursion" $ do
            --     let source = unlines
            --             [ "factorial : Int -> Int"
            --             , "factorial n ="
            --             , "    if n <= 1 then 1 else n * factorial (n - 1)"
            --             ]
            --     let tokens = either (const []) id $ scanTokens source
            --     let filteredTokens = filter (not . isSkippable . tokenType) tokens
            --     case parseModule filteredTokens of
            --         Right (Module _ _ _ decls) -> do
            --             -- 1 Annotation + 1 Function Body
            --             length decls `shouldBe` 2
            --         Left err -> expectationFailure $ "Recursion parse failed: " ++ show err

            -- it "parses type aliases" $ do
            --     let source = "type alias Name = String"
            --     let tokens = either (const []) id $ scanTokens source
            --     let filtered = filter (not . isSkippable . tokenType) tokens
            --     case parseModule filtered of
            --         Right (Module _ _ _ [TypeAlias "Name" [] _]) -> True `shouldBe` True
            --         _ -> expectationFailure "Failed to parse TypeAlias"
-- Helper to extract name from a declaration for testing
declName :: Declaration -> String
declName (TypeAnnotation name _) = name
declName (FunctionDecl name _ _) = name
declName (TypeDecl name _ _)    = name
declName (TypeAlias name _ _)   = name
-- Helper to check if token should be skipped
isSkippable :: TokenType -> Bool
isSkippable (COMMENT _) = True
isSkippable NEWLINE = True
isSkippable _ = False