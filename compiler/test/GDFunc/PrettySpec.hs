module GDFunc.PrettySpec (spec) where

import Test.Hspec
import GDFunc.Parser
import GDFunc.Pretty

spec :: Spec
spec = do
    describe "Pretty Printer" $ do
        it "pretty prints variables" $ do
            prettyExpr (EVar "foo") `shouldBe` "foo"
        
        it "pretty prints borrowed variables" $ do
            prettyExpr (EBorrow (EVar "list")) `shouldBe` "&list"

        it "pretty prints lists" $ do
            prettyExpr (EList [EInt 1, EInt 2]) `shouldBe` "[1, 2]"

        it "pretty prints types" $ do
            prettyType (TCon "List" [TVar "a"]) `shouldBe` "List a"

        it "pretty prints borrowed types" $ do
            prettyType (TBorrowed (TCon "List" [TVar "a"])) `shouldBe` "&(List a)"

        it "pretty prints shared types" $ do
            prettyType (TShared (TCon "Int" [])) `shouldBe` "shared Int"
        
        it "pretty prints linear arrows" $ do
            prettyType (TLinearArrow (TVar "a") (TVar "b")) `shouldBe` "a -o b"