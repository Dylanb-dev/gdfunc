-- | Source positions, spans, and annotated AST nodes.
--
-- This module is the long-term home for source-location information
-- used in error messages and tooling. The 'Position' here is distinct
-- from the legacy 'GDFunc.Scanner.Position' for now; they will be
-- consolidated when the scanner is ported to megaparsec.
module GDFunc.SrcSpan
    ( Position(..)
    , SrcSpan(..)
    , Located(..)
    , unLoc
    , locOf
    , spanFrom
    , spanMerge
    , noSpan
    ) where

-- | A point in a source file, 1-indexed.
data Position = Position
    { posLine   :: !Int
    , posColumn :: !Int
    }
    deriving stock (Eq, Ord, Show)

-- | An inclusive-start, exclusive-end span over a single source file.
data SrcSpan = SrcSpan
    { spanFile  :: !FilePath
    , spanStart :: !Position
    , spanEnd   :: !Position
    }
    deriving stock (Eq, Ord, Show)

-- | An AST node annotated with its source span.
data Located a = Located
    { locSpan :: !SrcSpan
    , locVal  :: a
    }
    deriving stock (Eq, Show, Functor, Foldable, Traversable)

unLoc :: Located a -> a
unLoc = locVal

locOf :: Located a -> SrcSpan
locOf = locSpan

spanFrom :: FilePath -> Position -> Position -> SrcSpan
spanFrom = SrcSpan

-- | Smallest span covering both inputs. Assumes a common file; takes
-- the first file otherwise.
spanMerge :: SrcSpan -> SrcSpan -> SrcSpan
spanMerge (SrcSpan f s1 e1) (SrcSpan _ s2 e2) =
    SrcSpan f (min s1 s2) (max e1 e2)

-- | Placeholder span for AST nodes that haven't been threaded yet.
noSpan :: SrcSpan
noSpan = SrcSpan "<unknown>" (Position 0 0) (Position 0 0)
