{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- | Type checker for GDFunc.
--
-- The borrow-tracking state machine in this module will be rebuilt on
-- top of 'GDFunc.TypeChecker.Borrow' (linear IO from @linear-base@)
-- in phase 2 of the ecosystem refactor — see that module's haddock.
module GDFunc.TypeChecker
    ( TypeError(..)
    , TypeEnv
    , LinearityError(..)
    , typeCheckModule
    , typeCheckExpr
    , inferType
    , checkLinearity
    , LinearResource(..)
    ) where

import GDFunc.Parser hiding (Type)
import qualified GDFunc.Parser as P
import Control.Monad.State
import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Prelude

-- Use linear types to represent resources that must be consumed exactly once

-- | A linear resource that must be used exactly once
data LinearResource a where
    LinearResource :: a %1 -> LinearResource a

-- | Consume a linear resource
consumeLinear :: LinearResource a %1 -> a
consumeLinear (LinearResource x) = x

-- Ur wrapper for unrestricted values in linear context
data Ur a where
    Ur :: a -> Ur a

unUr :: Ur a -> a
unUr (Ur x) = x

-- Type context. Primitives (Int, Float, String, Bool, Char) are
-- implicitly copyable per the language spec; every other type is
-- linear by default. There is no user-facing "shared" keyword.
data TypeContext = TypeContext
    { ctxBindings :: Map String P.Type
    , ctxLinearVars :: Set String      -- Variables that are linear (default)
    , ctxBorrowedVars :: Set String    -- Variables that are borrowed (&var)
    , ctxUsageCount :: Map String Int
    } deriving (Show)

-- | The set of primitive type names that are implicitly copyable.
copyablePrimitives :: Set String
copyablePrimitives = Set.fromList ["Int", "Float", "String", "Bool", "Char"]

-- | A type is copyable iff it is one of the primitives.
isCopyableType :: P.Type -> Bool
isCopyableType (P.TCon name _) = Set.member name copyablePrimitives
isCopyableType (P.TBorrowed _) = True  -- borrows can be reborrowed freely
isCopyableType _               = False

-- Simplified type environment
data LinTypeEnv = LinTypeEnv
    { envContext :: TypeContext
    , envFreshCounter :: Int
    } deriving (Show)

-- | Create an empty linear type environment. Primitives are
-- implicitly copyable (Int, Float, String, Bool, Char) — no spec
-- "shared" wrapper.
emptyLinEnv :: LinTypeEnv
emptyLinEnv = LinTypeEnv
    { envContext = TypeContext
        { ctxBindings = Map.fromList
            [ ("+",  intBinop)
            , ("-",  intBinop)
            , ("*",  intBinop)
            , ("/",  intBinop)
            , ("<=", intCompareOp)
            , (">=", intCompareOp)
            , ("<",  intCompareOp)
            , (">",  intCompareOp)
            , ("==", intCompareOp)
            , ("True",  P.TCon "Bool" [])
            , ("False", P.TCon "Bool" [])
            ]
        , ctxLinearVars   = Set.empty
        , ctxBorrowedVars = Set.empty
        , ctxUsageCount   = Map.empty
        }
    , envFreshCounter = 0
    }
  where
    int  = P.TCon "Int"  []
    bool = P.TCon "Bool" []
    intBinop     = P.TArrow int  (P.TArrow int int)
    intCompareOp = P.TArrow int  (P.TArrow int bool)

-- Type errors
data TypeError
    = UnboundVariable String
    | TypeMismatch { expected :: P.Type, actual :: P.Type }
    | LinearityViolation String LinearityError
    | OccursCheck String P.Type
    | UnificationError P.Type P.Type
    | NotAFunction P.Type
    | InvalidPattern P.Pattern
    | UnusedLinearVariable String
    | LinearVariableUsedMultipleTimes String Int
    | NonLinearUseOfLinearVariable String
    | CannotInferType
    deriving (Show, Eq)

data LinearityError
    = NotUsed
    | UsedMultipleTimes Int
    | UsedInNonLinearContext
    deriving (Show, Eq)

-- | Type checking monad - using standard monad, not linear
newtype LinTypeCheck a = LinTypeCheck 
    { runLinTypeCheck :: LinTypeEnv -> Either TypeError (a, LinTypeEnv) }

instance Functor LinTypeCheck where
    fmap f (LinTypeCheck m) = LinTypeCheck (\env ->
        case m env of
            Left err -> Left err
            Right (x, env') -> Right (f x, env'))

instance Applicative LinTypeCheck where
    pure x = LinTypeCheck (\env -> Right (x, env))
    
    LinTypeCheck mf <*> LinTypeCheck mx = LinTypeCheck (\env ->
        case mf env of
            Left err -> Left err
            Right (f, env') -> 
                case mx env' of
                    Left err -> Left err
                    Right (x, env'') -> Right (f x, env''))

instance Monad LinTypeCheck where
    LinTypeCheck m >>= f = LinTypeCheck (\env ->
        case m env of
            Left err -> Left err
            Right (x, env') -> runLinTypeCheck (f x) env')

-- Linear environment operations

markLinear :: String -> LinTypeCheck ()
markLinear name = LinTypeCheck (\(LinTypeEnv ctx counter) ->
    let newCtx = ctx { ctxLinearVars = Set.insert name (ctxLinearVars ctx) }
    in Right ((), LinTypeEnv newCtx counter))

markBorrowed :: String -> LinTypeCheck ()
markBorrowed name = LinTypeCheck (\(LinTypeEnv ctx counter) ->
    let newCtx = ctx { ctxBorrowedVars = Set.insert name (ctxBorrowedVars ctx) }
    in Right ((), LinTypeEnv newCtx counter))

-- | A type is copyable (per spec, only primitives) iff
-- 'isCopyableType' says so. Lifted into 'LinTypeCheck' for convenience.
isTypeCopyable :: P.Type -> LinTypeCheck Bool
isTypeCopyable = pure . isCopyableType

isVarBorrowed :: String -> LinTypeCheck Bool
isVarBorrowed name = LinTypeCheck (\env ->
    let borrowed = Set.member name (ctxBorrowedVars (envContext env))
    in Right (borrowed, env))

markUsed :: String -> LinTypeCheck ()
markUsed name = LinTypeCheck (\(LinTypeEnv ctx counter) ->
    let currentCount = Map.findWithDefault 0 name (ctxUsageCount ctx)
        newUsage = Map.insert name (currentCount + 1) (ctxUsageCount ctx)
        newCtx = ctx { ctxUsageCount = newUsage }
    in Right ((), LinTypeEnv newCtx counter))

lookupVar :: String -> LinTypeCheck P.Type
lookupVar name = LinTypeCheck (\env ->
    case Map.lookup name (ctxBindings (envContext env)) of
        Nothing -> Left (UnboundVariable name)
        Just typ -> Right (typ, env))

extendEnv :: String -> P.Type -> LinTypeCheck ()
extendEnv name typ = LinTypeCheck (\(LinTypeEnv ctx counter) ->
    let newBindings = Map.insert name typ (ctxBindings ctx)
        newCtx = ctx { ctxBindings = newBindings }
    in Right ((), LinTypeEnv newCtx counter))

freshTyVar :: LinTypeCheck P.Type
freshTyVar = LinTypeCheck (\(LinTypeEnv ctx n) ->
    let newVar = P.TVar ("t" ++ show n)
        newCounter = n + 1
    in Right (newVar, LinTypeEnv ctx newCounter))

checkLinearityInScope :: LinTypeCheck ()
checkLinearityInScope = LinTypeCheck (\env ->
    let ctx = envContext env
        linVars = ctxLinearVars ctx
        borrowedVars = ctxBorrowedVars ctx
        usage = ctxUsageCount ctx
        -- Only check linear variables that are not borrowed
        linVarsToCheck = Set.difference linVars borrowedVars
        errors = Set.foldr (\var acc ->
            let count = Map.findWithDefault 0 var usage
            in case count of
                0 -> Left (LinearityViolation var NotUsed) : acc
                1 -> acc
                n -> Left (LinearityViolation var (UsedMultipleTimes n)) : acc
            ) [] linVarsToCheck
    in case errors of
        (Left err : _) -> Left err
        _ -> Right ((), env))

-- Simple linear resource example (no IO)
data LinearFile where
    LinearFile :: String -> LinearFile

makeLinearFile :: String -> LinearResource LinearFile
makeLinearFile path = LinearResource (LinearFile path)

consumeLinearFile :: LinearFile -> String
consumeLinearFile (LinearFile path) = path

-- Expression type inference

inferExpr :: P.Expr -> LinTypeCheck P.Type
inferExpr (P.EVar name) = do
    -- Check if the variable is borrowed or has a copyable primitive type
    borrowed <- isVarBorrowed name
    varType <- lookupVar name
    copyable <- isTypeCopyable varType

    -- Only track usage for linear variables (not borrowed or copyable)
    if not borrowed && not copyable
        then markUsed name
        else return ()

    return varType

inferExpr (P.EBorrow expr) = do
    -- Borrowing creates a non-consuming reference
    exprType <- inferExpr expr
    return (P.TBorrowed exprType)

-- Primitive literals are implicitly copyable (per language spec).
inferExpr (P.EInt _)    = return (P.TCon "Int"    [])
inferExpr (P.EFloat _)  = return (P.TCon "Float"  [])
inferExpr (P.EChar _)   = return (P.TCon "Char"   [])
inferExpr (P.EString _) = return (P.TCon "String" [])

inferExpr (P.EList []) = do
    tv <- freshTyVar
    return (P.TCon "List" [tv])

inferExpr (P.EList (e:es)) = do
    t1 <- inferExpr e
    tRest <- inferExpr (P.EList es)
    case tRest of
        P.TCon "List" [elemType] -> 
            return (P.TCon "List" [t1])
        _ -> LinTypeCheck (\env -> Left (TypeMismatch (P.TCon "List" [t1]) tRest))

inferExpr (P.ELambda patterns body) = do
    savedEnv <- saveEnv
    
    patResults <- mapM inferPattern patterns
    let patTypes = map fst patResults
    let patBindings = concatMap snd patResults
    
    mapM_ (\(name, typ) -> extendEnv name typ) patBindings
    
    bodyType <- inferExpr body
    
    checkLinearityInScope
    
    restoreEnv savedEnv
    
    return (foldr P.TArrow bodyType patTypes)

inferExpr (P.EApp f arg) = do
    funcType <- inferExpr f
    argType <- inferExpr arg
    resultType <- freshTyVar
    return resultType

inferExpr (P.ELet bindings body) = do
    savedEnv <- saveEnv
    
    mapM_ processLetBinding bindings
    
    bodyType <- inferExpr body
    
    checkLinearityInScope
    
    restoreEnv savedEnv
    
    return bodyType

inferExpr (P.EIf cond thenE elseE) = do
    condType <- inferExpr cond
    thenType <- inferExpr thenE
    elseType <- inferExpr elseE
    return thenType

inferExpr (P.ECase scrutinee branches) = do
    scrutineeType <- inferExpr scrutinee
    branchTypes <- mapM (checkBranch scrutineeType) branches
    case branchTypes of
        [] -> LinTypeCheck (\env -> Left CannotInferType)
        (t:_) -> return t

inferExpr (P.ETuple exprs) = do
    types <- mapM inferExpr exprs
    return (P.TTuple types)

inferExpr (P.ERecord fields) = do
    fieldTypes <- mapM (\(n, e) -> do
        t <- inferExpr e
        return (n, t)) fields
    return (P.TRecord fieldTypes Nothing)

inferExpr (P.ERecordUpdate name fields) = do
    recordType <- lookupVar name
    return recordType

inferExpr (P.EFieldAccess expr field) = do
    exprType <- inferExpr expr
    freshTyVar

inferExpr (P.EBinOp op left right) = do
    leftType <- inferExpr left
    rightType <- inferExpr right
    
    -- Look up operator if it exists in the environment
    opTypeMaybe <- LinTypeCheck (\env ->
        case Map.lookup op (ctxBindings (envContext env)) of
            Nothing -> Right (Nothing, env)
            Just typ -> Right (Just typ, env))
    
    case opTypeMaybe of
        Just opType -> do
            -- For now, just return the result type
            -- A proper implementation would unify types
            case opType of
                P.TArrow _ (P.TArrow _ resultType) -> return resultType
                _ -> return leftType
        Nothing -> return leftType

inferExpr (P.EParens expr) = inferExpr expr

inferExpr expr = freshTyVar

-- Helper functions

inferPattern :: P.Pattern -> LinTypeCheck (P.Type, [(String, P.Type)])
inferPattern (P.PVar name) = do
    tv <- freshTyVar
    -- Don't mark as linear yet - linearity determined by actual type
    return (tv, [(name, tv)])

inferPattern (P.PBorrow pattern) = do
    (patType, bindings) <- inferPattern pattern
    -- Mark all variables in the pattern as borrowed
    mapM_ (\(name, _) -> markBorrowed name) bindings
    return (P.TBorrowed patType, bindings)

inferPattern P.PWildcard = do
    tv <- freshTyVar
    return (tv, [])

-- Primitive patterns are implicitly copyable.
inferPattern (P.PInt _)    = return (P.TCon "Int"    [], [])
inferPattern (P.PFloat _)  = return (P.TCon "Float"  [], [])
inferPattern (P.PChar _)   = return (P.TCon "Char"   [], [])
inferPattern (P.PString _) = return (P.TCon "String" [], [])

inferPattern (P.PCons p1 p2) = do
    (t1, bindings1) <- inferPattern p1
    (t2, bindings2) <- inferPattern p2
    return (P.TCon "List" [t1], bindings1 ++ bindings2)

inferPattern (P.PList patterns) = do
    results <- mapM inferPattern patterns
    let types = map fst results
    let bindings = concatMap snd results
    case types of
        [] -> do
            tv <- freshTyVar
            return (P.TCon "List" [tv], bindings)
        (t:_) -> return (P.TCon "List" [t], bindings)

inferPattern (P.PTuple patterns) = do
    results <- mapM inferPattern patterns
    let types = map fst results
    let bindings = concatMap snd results
    return (P.TTuple types, bindings)

inferPattern (P.PParens pat) = inferPattern pat

inferPattern (P.PRecord fields) = do
    tv <- freshTyVar
    return (tv, [])

inferPattern (P.PCtor name patterns) = do
    tv <- freshTyVar
    return (tv, [])

inferPattern (P.PAs name pat) = do
    (patType, bindings) <- inferPattern pat
    return (patType, (name, patType) : bindings)

inferPattern pat = do
    tv <- freshTyVar
    return (tv, [])

processLetBinding :: P.LetBinding -> LinTypeCheck ()
processLetBinding (P.LetAnnotation name typ) = extendEnv name typ

processLetBinding (P.LetDef name patterns expr) = do
    patResults <- mapM inferPattern patterns
    let patTypes = map fst patResults
    bodyType <- inferExpr expr
    let funcType = foldr P.TArrow bodyType patTypes
    extendEnv name funcType

processLetBinding (P.LetDestructure pattern expr) = do
    (patType, bindings) <- inferPattern pattern
    exprType <- inferExpr expr
    -- Add bindings and mark as linear only if the type isn't copyable
    mapM_ (\(name, typ) -> do
        extendEnv name typ
        copyable <- isTypeCopyable exprType
        if not copyable then markLinear name else return ()
        ) bindings

checkBranch :: P.Type -> (P.Pattern, P.Expr) -> LinTypeCheck P.Type
checkBranch scrutineeType (pattern, expr) = do
    savedEnv <- saveEnv
    (patType, bindings) <- inferPattern pattern
    mapM_ (\(name, typ) -> extendEnv name typ) bindings
    exprType <- inferExpr expr
    restoreEnv savedEnv
    return exprType

saveEnv :: LinTypeCheck LinTypeEnv
saveEnv = LinTypeCheck (\env -> Right (env, env))

restoreEnv :: LinTypeEnv -> LinTypeCheck ()
restoreEnv newEnv = LinTypeCheck (\_ -> Right ((), newEnv))

-- Public API

typeCheckExpr :: P.Expr -> Either TypeError P.Type
typeCheckExpr expr = 
    case runLinTypeCheck (inferExpr expr) emptyLinEnv of
        Left err -> Left err
        Right (typ, _) -> Right typ

typeCheckModule :: P.Module -> Either TypeError ()
typeCheckModule (P.Module _ _ _ decls) = 
    case runLinTypeCheck (mapM_ checkDecl decls) emptyLinEnv of
        Left err -> Left err
        Right _ -> Right ()

checkDecl :: P.Declaration -> LinTypeCheck ()
checkDecl (P.TypeAnnotation name typ) = extendEnv name typ

checkDecl (P.TypeDecl _name _typeVars _constructors) =
    -- User-defined types are linear by default (per spec); no flag to record.
    return ()

checkDecl (P.TypeAlias _name _typeVars _typ) =
    -- Aliases inherit the underlying type's linearity.
    return ()

checkDecl (P.FunctionDecl name patterns expr) = do
    -- Create a placeholder type for the function to support recursion
    placeholderTypes <- mapM (const freshTyVar) patterns
    resultType <- freshTyVar
    let placeholderFuncType = foldr P.TArrow resultType placeholderTypes

    -- Add function to environment FIRST (for recursion)
    extendEnv name placeholderFuncType

    -- Infer pattern types and bind variables
    patResults <- mapM inferPattern patterns
    let patTypes = map fst patResults
    let patBindings = concatMap snd patResults

    -- Add pattern bindings to environment
    mapM_ (\(varName, varType) -> extendEnv varName varType) patBindings

    -- Infer body type
    bodyType <- inferExpr expr

    -- Build actual function type
    let funcType = foldr P.TArrow bodyType patTypes

    -- Update function in environment with correct type
    extendEnv name funcType

inferType :: P.Expr -> Either TypeError P.Type
inferType = typeCheckExpr

checkLinearity :: P.Expr -> Either TypeError ()
checkLinearity expr =
    case runLinTypeCheck (inferExpr expr >> checkLinearityInScope) emptyLinEnv of
        Left err -> Left err
        Right _ -> Right ()

-- Old non-linear API for compatibility
data TypeEnv = TypeEnv
    { typeBindings :: Map String P.Type
    , linearVars :: Set String
    , usedVars :: Map String Int
    }

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty Set.empty Map.empty

builtinEnv :: TypeEnv
builtinEnv = TypeEnv
    { typeBindings = Map.fromList
        [ ("+",  intBinop)
        , ("-",  intBinop)
        , ("*",  intBinop)
        , ("/",  intBinop)
        , ("++", P.TArrow (P.TCon "List" [P.TVar "a"]) (P.TArrow (P.TCon "List" [P.TVar "a"]) (P.TCon "List" [P.TVar "a"])))
        , ("::", P.TArrow (P.TVar "a") (P.TArrow (P.TCon "List" [P.TVar "a"]) (P.TCon "List" [P.TVar "a"])))
        , ("True",  P.TCon "Bool" [])
        , ("False", P.TCon "Bool" [])
        ]
    , linearVars = Set.empty
    , usedVars  = Map.empty
    }
  where
    int = P.TCon "Int" []
    intBinop = P.TArrow int (P.TArrow int int)
