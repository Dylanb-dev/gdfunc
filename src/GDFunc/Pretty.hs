module GDFunc.Pretty
    ( prettyModule
    , prettyExpr
    , prettyType
    , prettyPattern
    ) where

import GDFunc.Parser 
    ( Expr(..)
    , Type(..)
    , Pattern(..)
    , Declaration(..)
    , Module(..)
    , Import(..)
    , LetBinding(..)  -- Explicitly import LetBinding with all constructors
    )
import Data.List (intercalate)

-- Pretty print a module
prettyModule :: Module -> String
prettyModule (Module name exp imports decls) =
    "module " ++ intercalate "." name ++
    maybe "" (\e -> " exposing (" ++ intercalate ", " e ++ ")") exp ++ "\n\n" ++
    concatMap (\i -> prettyImport i ++ "\n") imports ++
    (if null imports then "" else "\n") ++
    intercalate "\n\n" (map prettyDeclaration decls)

prettyImport :: Import -> String
prettyImport (Import name alias exp) =
    "import " ++ intercalate "." name ++
    maybe "" (" as " ++) alias ++
    maybe "" (\e -> " exposing (" ++ intercalate ", " e ++ ")") exp

-- Pretty print a declaration
prettyDeclaration :: Declaration -> String
prettyDeclaration (TypeAnnotation name typ) =
    name ++ " : " ++ prettyType typ

prettyDeclaration (FunctionDecl name patterns expr) =
    name ++ " " ++ unwords (map prettyPattern patterns) ++ " =\n" ++
    indent (prettyExpr expr)

prettyDeclaration (TypeDecl name vars ctors) =
    "type " ++ name ++ " " ++ unwords vars ++ "\n" ++
    indent ("= " ++ intercalate "\n| " (map prettyCtor ctors))
  where
    prettyCtor (n, types) = n ++ " " ++ unwords (map prettyTypeAtom types)

prettyDeclaration (TypeAlias name vars typ) =
    "type alias " ++ name ++ " " ++ unwords vars ++ " =\n" ++
    indent (prettyType typ)

-- Pretty print a type
prettyType :: Type -> String
prettyType (TVar name) = name
prettyType (TLinear typ) = prettyTypeAtom typ ++ "!"
prettyType (TCon name []) = name
prettyType (TCon name args) = name ++ " " ++ unwords (map prettyTypeAtom args)
prettyType (TArrow left right) = prettyTypeAtom left ++ " -> " ++ prettyType right
prettyType (TLinearArrow left right) = prettyTypeAtom left ++ " -o " ++ prettyType right
prettyType (TTuple types) = "(" ++ intercalate ", " (map prettyType types) ++ ")"
prettyType (TRecord fields Nothing) =
    "{ " ++ intercalate ", " (map prettyField fields) ++ " }"
  where
    prettyField (n, t) = n ++ " : " ++ prettyType t
prettyType (TRecord fields (Just ext)) =
    "{ " ++ ext ++ " | " ++ intercalate ", " (map prettyField fields) ++ " }"
  where
    prettyField (n, t) = n ++ " : " ++ prettyType t

prettyTypeAtom :: Type -> String
prettyTypeAtom t@(TVar _) = prettyType t
prettyTypeAtom t@(TCon _ []) = prettyType t
prettyTypeAtom t = "(" ++ prettyType t ++ ")"

-- Pretty print an expression
prettyExpr :: Expr -> String
prettyExpr (EVar name) = name
prettyExpr (ELinearVar name) = name ++ "!"
prettyExpr (EInt n) = show n
prettyExpr (EFloat f) = show f
prettyExpr (EChar c) = show c
prettyExpr (EString s) = show s
prettyExpr (EList exprs) = "[" ++ intercalate ", " (map prettyExpr exprs) ++ "]"
prettyExpr (ETuple exprs) = "(" ++ intercalate ", " (map prettyExpr exprs) ++ ")"
prettyExpr (ERecord fields) =
    "{ " ++ intercalate ", " (map prettyField fields) ++ " }"
  where
    prettyField (n, e) = n ++ " = " ++ prettyExpr e
prettyExpr (ERecordUpdate name fields) =
    "{ " ++ name ++ " | " ++ intercalate ", " (map prettyField fields) ++ " }"
  where
    prettyField (n, e) = n ++ " = " ++ prettyExpr e

prettyExpr (EIf cond thenE elseE) =
    "if " ++ prettyExpr cond ++ " then\n" ++
    indent (prettyExpr thenE) ++ "\n" ++
    "else\n" ++
    indent (prettyExpr elseE)

prettyExpr (ECase isLinear scrutinee branches) =
    (if isLinear then "case! " else "case ") ++ prettyExpr scrutinee ++ " of\n" ++
    indent (intercalate "\n\n" (map prettyBranch branches))
  where
    prettyBranch (pat, expr) =
        prettyPattern pat ++ " ->\n" ++ indent (prettyExpr expr)

prettyExpr (ELet bindings body) =
    "let\n" ++
    indent (intercalate "\n\n" (map prettyBinding bindings)) ++ "\n" ++
    "in\n" ++
    indent (prettyExpr body)
  where
    prettyBinding (LetAnnotation name typ) = name ++ " : " ++ prettyType typ
    prettyBinding (LetDef name patterns expr) =
        name ++ " " ++ unwords (map prettyPattern patterns) ++ " =\n" ++
        indent (prettyExpr expr)
    prettyBinding (LetDestructure pat expr) =
        prettyPattern pat ++ " =\n" ++ indent (prettyExpr expr)

prettyExpr (ELambda patterns body) =
    "\\" ++ unwords (map prettyPattern patterns) ++ " -> " ++ prettyExpr body

prettyExpr (EApp f arg) = prettyExpr f ++ " " ++ prettyExprAtom arg

prettyExpr (EBinOp op left right) =
    prettyExprAtom left ++ " " ++ op ++ " " ++ prettyExprAtom right

prettyExpr (EFieldAccess expr field) = prettyExpr expr ++ "." ++ field

prettyExpr (EParens expr) = "(" ++ prettyExpr expr ++ ")"

prettyExprAtom :: Expr -> String
prettyExprAtom e@(EVar _) = prettyExpr e
prettyExprAtom e@(ELinearVar _) = prettyExpr e
prettyExprAtom e@(EInt _) = prettyExpr e
prettyExprAtom e@(EFloat _) = prettyExpr e
prettyExprAtom e@(EChar _) = prettyExpr e
prettyExprAtom e@(EString _) = prettyExpr e
prettyExprAtom e@(EList _) = prettyExpr e
prettyExprAtom e@(ETuple _) = prettyExpr e
prettyExprAtom e@(ERecord _) = prettyExpr e
prettyExprAtom e = "(" ++ prettyExpr e ++ ")"

-- Pretty print a pattern
prettyPattern :: Pattern -> String
prettyPattern (PVar name) = name
prettyPattern (PLinearVar name) = name ++ "!"
prettyPattern PWildcard = "_"
prettyPattern (PInt n) = show n
prettyPattern (PFloat f) = show f
prettyPattern (PChar c) = show c
prettyPattern (PString s) = show s
prettyPattern (PCons left right) = prettyPattern left ++ " :: " ++ prettyPattern right
prettyPattern (PList patterns) = "[" ++ intercalate ", " (map prettyPattern patterns) ++ "]"
prettyPattern (PTuple patterns) = "(" ++ intercalate ", " (map prettyPattern patterns) ++ ")"
prettyPattern (PRecord fields) = "{ " ++ intercalate ", " fields ++ " }"
prettyPattern (PCtor name []) = name
prettyPattern (PCtor name patterns) = name ++ " " ++ unwords (map prettyPatternAtom patterns)
prettyPattern (PAs name pat) = name ++ " as " ++ prettyPattern pat
prettyPattern (PParens pat) = "(" ++ prettyPattern pat ++ ")"

prettyPatternAtom :: Pattern -> String
prettyPatternAtom p@(PVar _) = prettyPattern p
prettyPatternAtom p@(PLinearVar _) = prettyPattern p
prettyPatternAtom p@PWildcard = prettyPattern p
prettyPatternAtom p@(PInt _) = prettyPattern p
prettyPatternAtom p@(PFloat _) = prettyPattern p
prettyPatternAtom p@(PChar _) = prettyPattern p
prettyPatternAtom p@(PString _) = prettyPattern p
prettyPatternAtom p@(PList _) = prettyPattern p
prettyPatternAtom p@(PTuple _) = prettyPattern p
prettyPatternAtom p@(PRecord _) = prettyPattern p
prettyPatternAtom p = "(" ++ prettyPattern p ++ ")"  -- Changed from prettyPattern to prettyPatternAtom

-- Utility
indent :: String -> String
indent s = unlines $ map ("    " ++) (lines s)