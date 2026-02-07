{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QualifiedDo #-}

module GDFunc.TypeChecker
  ( typeCheckExpr
  , checkLinearity
  , TypeError(..)
  , LinearityError(..)
  ) where

import GDFunc.Parser
import Prelude.Linear hiding (consume)
import qualified Prelude
import qualified Data.Map.Strict as M
import qualified Data.Set as S
-- Use the linear version of Monad operators
import qualified Control.Functor.Linear as Control
import Data.Unrestricted.Linear (Ur(..))

------------------------------------------------------------
-- Errors
------------------------------------------------------------

data TypeError
  = UnboundVariable String
  | TypeMismatch Type Type
  | UnificationError Type Type
  | NotAFunction Type
  | LinearityViolation String LinearityError
  deriving (Prelude.Show)

data LinearityError
  = NotUsed
  | UsedMultipleTimes Int
  deriving (Prelude.Show)

------------------------------------------------------------
-- Linear environment
------------------------------------------------------------

data LEnv where
  LEmpty :: LEnv
  LBind  :: String -> LEnv %1 -> LEnv

-- helper to consume a specific variable from the linear stack
consume :: String -> LEnv %1 -> Either TypeError LEnv
consume x = \case
  LEmpty -> Left (LinearityViolation x NotUsed)
  LBind y env
    | x == y    -> Right env
    | otherwise -> 
        -- We must thread the result of the recursive call back into a new LBind
        case consume x env of
          Left err -> Left err
          Right env' -> Right (LBind y env')

------------------------------------------------------------
-- Linearity checking
------------------------------------------------------------

checkLinearity :: Expr -> Either TypeError ()
checkLinearity e =
  case check S.empty LEmpty e of
    Left err -> Left err
    Right env -> case env of
      LEmpty -> Right ()
      LBind x _ -> Left (LinearityViolation x NotUsed)

------------------------------------------------------------
-- Linearity checking
------------------------------------------------------------

-- Helper to extract expressions from let-bindings for the folder.
bindingExpr :: LetBinding -> Expr
bindingExpr = \case
  LetDef _ _ e -> e
  LetDestructure _ e -> e
  _ -> EVar "()"
------------------------------------------------------------
-- 1. Linear Helpers (Defined at Top Level)
------------------------------------------------------------
------------------------------------------------------------
-- Top-Level Helpers (No closures allowed)
------------------------------------------------------------

-- | Replaces (check linear) to avoid unrestricted closures
checkStep :: S.Set String -> LEnv %1 -> Expr -> Either TypeError LEnv
checkStep linear env e = check linear env e

-- | Explicitly linear step for lambda variable cleanup
step :: LEnv %1 -> String -> Either TypeError LEnv
step e x = case consume x e of
  Left _   -> Left (LinearityViolation x NotUsed)
  Right e' -> Right e'

-- | Multiplicity-polymorphic fold
foldLinear :: (LEnv %1 -> a -> Either TypeError LEnv) -> LEnv %1 -> [a] -> Either TypeError LEnv
foldLinear _ env [] = Right env
foldLinear f env (x:xs) = case f env x of
  Left err -> Left err
  Right env' -> foldLinear f env' xs

------------------------------------------------------------
-- The Main Check Function
------------------------------------------------------------

-- Ensure the signature uses the linear arrow %1
check :: S.Set String -> LEnv %1 -> Expr -> Either TypeError LEnv
check linear env expr = case expr of
  ELinearVar x -> consume x env
  
  EVar _ -> Right env

  ELambda pats body ->
    let linearVars = [ x | PLinearVar x <- pats ]
        envWithBinds = Prelude.foldr LBind env linearVars
    in case check (S.union linear (S.fromList linearVars)) envWithBinds body of
         Left err -> Left err
         Right envAfterBody -> foldLinear step envAfterBody linearVars

  EApp f x ->
    case check linear env f of
      Left err -> Left err
      Right env' -> check linear env' x

  EBinOp _ a b ->
    case check linear env a of
      Left err -> Left err
      Right env' -> check linear env' b

  EIf c t e ->
    case check linear env c of
      Left err -> Left err
      Right env' ->
        case check linear env' t of
          Left err -> Left err
          Right env'' -> check linear env'' e

  ETuple es -> 
    foldLinear (checkStep linear) env es

  EList es -> 
    foldLinear (checkStep linear) env es

  ELet binds body ->
    case foldLinear (checkStep linear) env (Prelude.map bindingExpr binds) of
      Left err -> Left err
      Right envAfterBinds -> check linear envAfterBinds body

  EParens e -> check linear env e

  _ -> Right env
------------------------------------------------------------
-- Type inference (Unrestricted/Standard Logic)
------------------------------------------------------------

typeCheckExpr :: Expr -> Either TypeError Type
typeCheckExpr = infer M.empty

infer :: M.Map String Type -> Expr -> Either TypeError Type
infer env = \case
  EInt _    -> Right (TCon "Int" [])
  EFloat _  -> Right (TCon "Float" [])
  EChar _   -> Right (TCon "Char" [])
  EString _ -> Right (TCon "String" [])

  EVar x ->
    maybe (Left (UnboundVariable x)) Right (M.lookup x env)

  ELinearVar x ->
    maybe (Left (UnboundVariable x)) Right (M.lookup x env)

  ELambda pats body ->
    let args = Prelude.replicate (Prelude.length pats) (TVar "_a")
        names = [ n | PVar n <- pats ] Prelude.++ [ n | PLinearVar n <- pats ]
        env' = M.union (M.fromList (Prelude.zip names args)) env
    in infer env' body Prelude.>>= \res -> Right (Prelude.foldr TArrow res args)

  EApp f x -> do
    tf <- infer env f
    tx <- infer env x
    case tf of
      TArrow a b | a == tx -> Right b
      TLinearArrow a b | a == tx -> Right b
      TArrow a _ -> Left (TypeMismatch a tx)
      _ -> Left (NotAFunction tf)

  EList [] ->
    Right (TCon "List" [TVar "a"])

  EList (x:xs) -> do
    t <- infer env x
    Prelude.mapM (\e -> infer env e Prelude.>>= unify t) xs
    Right (TCon "List" [t])

  ETuple es ->
    Prelude.mapM (infer env) es Prelude.>>= \ts -> Right (TTuple ts)

  ERecord fs -> do
    ts <- Prelude.mapM (\(n,e) -> (n,) Prelude.<$> infer env e) fs
    Right (TRecord ts Prelude.Nothing)

  EFieldAccess e f -> do
    te <- infer env e
    case te of
      TRecord fs _ ->
        maybe (Left (UnificationError te te)) Right (Prelude.lookup f fs)
      _ -> Left (UnificationError te te)

  EParens e ->
    infer env e

  _ ->
    Right (TVar "_")

unify :: Type -> Type -> Either TypeError ()
unify a b
  | a == b    = Right ()
  | otherwise = Left (UnificationError a b)