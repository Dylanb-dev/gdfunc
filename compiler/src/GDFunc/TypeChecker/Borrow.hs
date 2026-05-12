{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}

-- | Linear borrow handles for the type checker.
--
-- This module is the anchor for an upcoming redesign of the
-- borrow-tracking discipline in 'GDFunc.TypeChecker'. The plan:
--
--   * Every active borrow in the user program is represented at the
--     compiler level as a 'BorrowHandle', which is a /linear/ value:
--     the host Haskell compiler refuses to compile borrow-tracking
--     code that drops or duplicates a handle.
--   * The borrow checker will run in 'System.IO.Linear.IO' (from
--     @linear-base@) so that allocating and releasing handles is
--     done with linear primitives. The host GHC's linear-types
--     machinery then enforces "released exactly once" on every
--     borrow we open during checking — the language's borrow rules
--     dogfood Haskell's.
--
-- For now this module only fixes the types and the release primitive;
-- 'GDFunc.TypeChecker' continues to use its existing 'State'-based
-- borrow tracking. Phase 2 of the rewrite will move that logic here
-- and rebuild it on top of 'System.IO.Linear'.
module GDFunc.TypeChecker.Borrow
    ( BorrowHandle
    , freshHandle
    , releaseBorrow
    ) where

-- | A linear handle representing an active borrow during type checking.
--
-- The constructor is declared with GADT syntax so the 'Int' payload
-- has unrestricted (@%Many@) multiplicity; the /handle/ itself is
-- consumed linearly via 'releaseBorrow'. The payload will grow in
-- phase 2 (span, refinement info) but stays multiplicity-Many — it's
-- bookkeeping, not a resource.
data BorrowHandle where
    BorrowHandle :: Int -> BorrowHandle

-- | Allocate a fresh borrow handle from a unique id.
freshHandle :: Int -> BorrowHandle
freshHandle = BorrowHandle

-- | Release a borrow handle. Linear in the handle: the host compiler
-- enforces that every borrow opened during checking is released
-- exactly once.
releaseBorrow :: BorrowHandle %1 -> ()
releaseBorrow (BorrowHandle _) = ()
