-- | Tests for the linear-IO borrow analyzer.
--
-- These build the AST by hand so the analyzer is exercised
-- independently of the parser (which today has known limitations
-- around multi-line bodies). They also document the contract: the
-- analyzer counts every 'EBorrow' / 'PBorrow' as one open-and-release
-- scope, all running inside 'System.IO.Linear.IO'.
module GDFunc.BorrowSpec (spec) where

import Test.Hspec

import qualified GDFunc.Parser as P
import qualified GDFunc.TypeChecker.Borrow as Borrow

-- Helper: a module with a single function declaration.
moduleWith :: [P.Declaration] -> P.Module
moduleWith decls = P.Module ["T"] Nothing [] decls

funDecl :: String -> [P.Pattern] -> P.Expr -> P.Declaration
funDecl = P.FunctionDecl

spec :: Spec
spec = describe "Linear-IO borrow analyzer" $ do
    it "reports zero borrows on an empty module" $ do
        r <- Borrow.borrowAnalyzeModule (moduleWith [])
        Borrow.reportOpened r   `shouldBe` 0
        Borrow.reportReleased r `shouldBe` 0

    it "ignores functions with no borrows" $ do
        let m = moduleWith
                [ funDecl "f" [P.PVar "x"] (P.EBinOp "+" (P.EVar "x") (P.EInt 1)) ]
        r <- Borrow.borrowAnalyzeModule m
        Borrow.reportOpened r `shouldBe` 0

    it "counts a borrow expression" $ do
        let m = moduleWith
                [ funDecl "g" [P.PVar "x"] (P.EBorrow (P.EVar "x")) ]
        r <- Borrow.borrowAnalyzeModule m
        Borrow.reportOpened   r `shouldBe` 1
        Borrow.reportReleased r `shouldBe` 1

    it "counts a borrow pattern" $ do
        let m = moduleWith
                [ funDecl "h" [P.PBorrow (P.PVar "list")] (P.EInt 0) ]
        r <- Borrow.borrowAnalyzeModule m
        Borrow.reportOpened r `shouldBe` 1

    it "counts nested borrows" $ do
        -- length &list   =   case list of [] -> 0 | _ :: rest -> 1 + length &rest
        let body = P.ECase (P.EVar "list")
                     [ (P.PList [], P.EInt 0)
                     , (P.PCons P.PWildcard (P.PVar "rest"),
                         P.EBinOp "+" (P.EInt 1)
                           (P.EApp (P.EVar "length") (P.EBorrow (P.EVar "rest"))))
                     ]
        let m = moduleWith [ funDecl "length" [P.PBorrow (P.PVar "list")] body ]
        r <- Borrow.borrowAnalyzeModule m
        -- 1 PBorrow (param) + 1 EBorrow (recursive call) = 2
        Borrow.reportOpened   r `shouldBe` 2
        Borrow.reportReleased r `shouldBe` 2

    it "opened equals released by construction (linear discipline)" $ do
        -- This is the invariant the host compiler enforces on us: if
        -- the analyzer ever drops a handle, this module fails to
        -- compile. So at runtime, opened must always equal released.
        let m = moduleWith
                [ funDecl "f" [P.PVar "x"]
                    (P.EBorrow (P.EBorrow (P.EBorrow (P.EVar "x")))) ]
        r <- Borrow.borrowAnalyzeModule m
        Borrow.reportOpened r `shouldBe` Borrow.reportReleased r
