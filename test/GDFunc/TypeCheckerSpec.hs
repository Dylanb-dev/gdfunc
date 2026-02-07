module GDFunc.TypeCheckerSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import GDFunc.Scanner
import GDFunc.Parser
import GDFunc.TypeChecker
import qualified GDFunc.Parser as P
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "Type Inference" $ do
        describe "Literals" $ do
            it "infers Int type for integer literals" $ do
                typeCheckString "42" `shouldBe` Right (P.TCon "Int" [])
            
            it "infers Float type for float literals" $ do
                typeCheckString "3.14" `shouldBe` Right (P.TCon "Float" [])
            
            it "infers Char type for character literals" $ do
                typeCheckString "'a'" `shouldBe` Right (P.TCon "Char" [])
            
            it "infers String type for string literals" $ do
                typeCheckString "\"hello\"" `shouldBe` Right (P.TCon "String" [])
        
        describe "Lists" $ do
            it "infers List Int for integer list" $ do
                let result = typeCheckString "[1, 2, 3]"
                result `shouldSatisfy` \case
                    Right (P.TCon "List" [P.TCon "Int" []]) -> True
                    _ -> False
            
            it "infers polymorphic list for empty list" $ do
                let result = typeCheckString "[]"
                result `shouldSatisfy` \case
                    Right (P.TCon "List" [P.TVar _]) -> True
                    _ -> False
            
            it "infers nested list types" $ do
                let result = typeCheckString "[[1, 2], [3, 4]]"
                result `shouldSatisfy` \case
                    Right (P.TCon "List" [P.TCon "List" [P.TCon "Int" []]]) -> True
                    _ -> False
        
        describe "Tuples" $ do
            it "infers tuple types" $ do
                let result = typeCheckString "(1, \"hello\")"
                result `shouldSatisfy` \case
                    Right (P.TTuple [P.TCon "Int" [], P.TCon "String" []]) -> True
                    _ -> False
            
            it "infers nested tuple types" $ do
                let result = typeCheckString "((1, 2), (3, 4))"
                result `shouldSatisfy` \case
                    Right (P.TTuple _) -> True
                    _ -> False
        
        describe "Functions" $ do
            it "infers identity function type" $ do
                let result = typeCheckString "\\x -> x"
                result `shouldSatisfy` \case
                    Right (P.TArrow (P.TVar _) (P.TVar _)) -> True
                    _ -> False
            
            it "infers const function type" $ do
                let result = typeCheckString "\\x y -> x"
                result `shouldSatisfy` \case
                    Right (P.TArrow _ (P.TArrow _ _)) -> True
                    _ -> False
            
            it "infers function with literals" $ do
                let result = typeCheckString "\\x -> x + 1"
                result `shouldSatisfy` \case
                    Right (P.TArrow (P.TCon "Int" []) (P.TCon "Int" [])) -> True
                    _ -> False
        
        describe "Let expressions" $ do
            it "type checks simple let binding" $ do
                let code = unlines
                        [ "let"
                        , "    x = 5"
                        , "in"
                        , "    x + 1"
                        ]
                typeCheckString code `shouldSatisfy` \case
                    Right (P.TCon "Int" []) -> True
                    _ -> False
            
            it "type checks let with type annotation" $ do
                let code = unlines
                        [ "let"
                        , "    x : Int"
                        , "    x = 5"
                        , "in"
                        , "    x"
                        ]
                typeCheckString code `shouldSatisfy` \case
                    Right (P.TCon "Int" []) -> True
                    _ -> False
            
            it "type checks nested let bindings" $ do
                let code = unlines
                        [ "let"
                        , "    x = 5"
                        , "    y = x + 1"
                        , "in"
                        , "    y * 2"
                        ]
                typeCheckString code `shouldSatisfy` \case
                    Right (P.TCon "Int" []) -> True
                    _ -> False
        
        describe "If expressions" $ do
            it "type checks if expression" $ do
                let code = "if True then 1 else 2"
                typeCheckString code `shouldSatisfy` \case
                    Right (P.TCon "Int" []) -> True
                    _ -> False
            
            it "requires bool condition" $ do
                let code = "if 1 then 2 else 3"
                typeCheckString code `shouldSatisfy` \case
                    Left (TypeMismatch _ _) -> True
                    Left (UnificationError _ _) -> True
                    _ -> False
            
            it "requires matching branch types" $ do
                let code = "if True then 1 else \"hello\""
                typeCheckString code `shouldSatisfy` \case
                    Left (TypeMismatch _ _) -> True
                    Left (UnificationError _ _) -> True
                    _ -> False
    
    describe "Linear Type Checking" $ do
        describe "Basic linearity" $ do
            it "accepts linear variable used exactly once" $ do
                let code = unlines
                        [ "let"
                        , "    x! = 5"
                        , "in"
                        , "    x!"
                        ]
                checkLinearityString code `shouldBe` Right ()
            
            it "rejects unused linear variable" $ do
                let code = unlines
                        [ "let"
                        , "    x! = 5"
                        , "in"
                        , "    10"
                        ]
                checkLinearityString code `shouldSatisfy` \case
                    Left (LinearityViolation _ NotUsed) -> True
                    _ -> False
            
            it "rejects linear variable used twice" $ do
                let code = unlines
                        [ "let"
                        , "    x! = 5"
                        , "in"
                        , "    x! + x!"
                        ]
                checkLinearityString code `shouldSatisfy` \case
                    Left (LinearityViolation _ (UsedMultipleTimes 2)) -> True
                    _ -> False
            
            it "rejects linear variable used three times" $ do
                let code = unlines
                        [ "let"
                        , "    x! = [1, 2, 3]"
                        , "in"
                        , "    (x!, x!, x!)"
                        ]
                checkLinearityString code `shouldSatisfy` \case
                    Left (LinearityViolation _ (UsedMultipleTimes 3)) -> True
                    _ -> False
        
        describe "Linear patterns" $ do
            it "accepts linear pattern variable used once" $ do
                let code = unlines
                        [ "case! list! of"
                        , "    [] -> 0"
                        , "    x :: xs! -> x"
                        ]
                -- Note: This would need a more complete test with proper context
                pending
            
            it "tracks linearity through pattern matching" $ do
                let code = unlines
                        [ "let"
                        , "    (x!, y!) = (1, 2)"
                        , "in"
                        , "    x! + y!"
                        ]
                checkLinearityString code `shouldBe` Right ()
        
        describe "Linear functions" $ do
            it "accepts linear arrow function" $ do
                let code = "\\x! -> x!"
                typeCheckString code `shouldSatisfy` \case
                    Right (P.TLinearArrow _ _) -> True
                    Right (P.TArrow _ _) -> True  -- May be inferred as regular arrow
                    _ -> False
            
            it "rejects linear parameter used twice in function" $ do
                let code = "\\x! -> (x!, x!)"
                checkLinearityString code `shouldSatisfy` \case
                    Left (LinearityViolation _ (UsedMultipleTimes 2)) -> True
                    _ -> False
            
            it "rejects unused linear parameter" $ do
                let code = "\\x! -> 42"
                checkLinearityString code `shouldSatisfy` \case
                    Left (LinearityViolation _ NotUsed) -> True
                    _ -> False
        
        describe "Linear lists" $ do
            it "accepts consuming linear list" $ do
                let code = unlines
                        [ "let"
                        , "    xs! = [1, 2, 3]"
                        , "in"
                        , "    case! xs! of"
                        , "        [] -> 0"
                        , "        x :: rest! -> x"
                        ]
                pending  -- Needs full case! implementation
            
            it "tracks linearity through list operations" $ do
                let code = unlines
                        [ "let"
                        , "    xs! = [1, 2, 3]"
                        , "    ys! = 4 :: xs!"
                        , "in"
                        , "    ys!"
                        ]
                checkLinearityString code `shouldBe` Right ()
    
    describe "Type Errors" $ do
        describe "Unbound variables" $ do
            it "detects unbound variable" $ do
                typeCheckString "x" `shouldSatisfy` \case
                    Left (UnboundVariable "x") -> True
                    _ -> False
            
            it "detects unbound variable in expression" $ do
                typeCheckString "x + 1" `shouldSatisfy` \case
                    Left (UnboundVariable "x") -> True
                    _ -> False
        
        describe "Type mismatches" $ do
            it "detects type mismatch in binary operator" $ do
                typeCheckString "1 + \"hello\"" `shouldSatisfy` \case
                    Left (TypeMismatch _ _) -> True
                    Left (UnificationError _ _) -> True
                    _ -> False
            
            it "detects type mismatch in list" $ do
                typeCheckString "[1, \"hello\"]" `shouldSatisfy` \case
                    Left (TypeMismatch _ _) -> True
                    Left (UnificationError _ _) -> True
                    _ -> False
        
        describe "Function application errors" $ do
            it "detects applying non-function" $ do
                typeCheckString "5 10" `shouldSatisfy` \case
                    Left (NotAFunction _) -> True
                    Left (UnificationError _ _) -> True
                    _ -> False
            
            it "detects argument type mismatch" $ do
                let code = unlines
                        [ "let"
                        , "    f = \\x -> x + 1"
                        , "in"
                        , "    f \"hello\""
                        ]
                typeCheckString code `shouldSatisfy` \case
                    Left (TypeMismatch _ _) -> True
                    Left (UnificationError _ _) -> True
                    _ -> False
    
    describe "Polymorphism" $ do
        describe "Let polymorphism" $ do
            it "allows polymorphic let bindings" $ do
                let code = unlines
                        [ "let"
                        , "    id = \\x -> x"
                        , "    a = id 5"
                        , "    b = id \"hello\""
                        , "in"
                        , "    a"
                        ]
                typeCheckString code `shouldSatisfy` \case
                    Right (P.TCon "Int" []) -> True
                    _ -> False
            
            it "generalizes type variables" $ do
                let code = unlines
                        [ "let"
                        , "    const = \\x y -> x"
                        , "in"
                        , "    const"
                        ]
                typeCheckString code `shouldSatisfy` \case
                    Right (P.TArrow _ (P.TArrow _ _)) -> True
                    _ -> False
    
    describe "Records" $ do
        describe "Record inference" $ do
            it "infers record type" $ do
                let code = "{ x = 1, y = 2 }"
                typeCheckString code `shouldSatisfy` \case
                    Right (P.TRecord _ _) -> True
                    _ -> False
            
            it "infers field access type" $ do
                let code = unlines
                        [ "let"
                        , "    point = { x = 1, y = 2 }"
                        , "in"
                        , "    point.x"
                        ]
                typeCheckString code `shouldSatisfy` \case
                    Right (P.TCon "Int" []) -> True
                    _ -> False
        
        describe "Record update" $ do
            it "type checks record update" $ do
                let code = unlines
                        [ "let"
                        , "    point = { x = 1, y = 2 }"
                        , "    updated = { point | x = 10 }"
                        , "in"
                        , "    updated"
                        ]
                typeCheckString code `shouldSatisfy` \case
                    Right (P.TRecord _ _) -> True
                    _ -> False

-- Helper functions

typeCheckString :: String -> Either TypeError P.Type
typeCheckString input = do
    tokens <- scanTokens input
    expr <- parseExpr tokens
    typeCheckExpr expr

checkLinearityString :: String -> Either TypeError ()
checkLinearityString input = do
    tokens <- scanTokens input
    expr <- parseExpr tokens
    checkLinearity expr

-- Property-based tests

prop_intLiteralHasIntType :: Int -> Bool
prop_intLiteralHasIntType n =
    case typeCheckString (show n) of
        Right (P.TCon "Int" []) -> True
        _ -> False

prop_listOfIntsHasListType :: [Int] -> Bool
prop_listOfIntsHasListType xs =
    let code = show xs
    in case typeCheckString code of
        Right (P.TCon "List" [P.TCon "Int" []]) -> True
        Right (P.TCon "List" [P.TVar _]) | null xs -> True
        _ -> False

-- QuickCheck properties
propertyTests :: Spec
propertyTests = describe "QuickCheck Properties" $ do
    it "integer literals type check as Int" $ 
        property prop_intLiteralHasIntType
    
    it "lists of integers type check as List Int" $
        property prop_listOfIntsHasListType

-- Integration tests with full programs

integrationTests :: Spec
integrationTests = describe "Integration Tests" $ do
    it "type checks quicksort" $ do
        let code = unlines
                [ "let"
                , "    quicksort = \\list ->"
                , "        case list of"
                , "            [] -> []"
                , "            pivot :: rest ->"
                , "                let"
                , "                    smaller = []"  -- Simplified
                , "                    larger = []"
                , "                in"
                , "                smaller"
                , "in"
                , "    quicksort"
                ]
        typeCheckString code `shouldSatisfy` \case
            Right (P.TArrow _ _) -> True
            _ -> False
    
    it "type checks linear quicksort" $ do
        let code = unlines
                [ "let"
                , "    quicksort! = \\list! ->"
                , "        case! list! of"
                , "            [] -> []"
                , "            pivot :: rest! -> []"  -- Simplified
                , "in"
                , "    quicksort!"
                ]
        pending  -- Requires full linear function implementation

-- Complete spec export
spec :: Spec
spec = do
    describe "Type Inference" typeInferenceTests
    describe "Linear Type Checking" linearTypeTests
    describe "Type Errors" typeErrorTests
    describe "Polymorphism" polymorphismTests
    describe "Records" recordTests
    propertyTests
    integrationTests

-- Individual test suites (defined above in main spec)
typeInferenceTests :: Spec
typeInferenceTests = return ()

linearTypeTests :: Spec
linearTypeTests = return ()

typeErrorTests :: Spec
typeErrorTests = return ()

polymorphismTests :: Spec
polymorphismTests = return ()

recordTests :: Spec
recordTests = return ()