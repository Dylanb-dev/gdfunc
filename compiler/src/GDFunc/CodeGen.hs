-- | Top-level code-generation dispatcher.
--
-- Backends live in 'GDFunc.CodeGen.C' and 'GDFunc.CodeGen.GDScript'.
-- This module exposes a small target-agnostic surface ('Target',
-- 'generate', 'compileToFile') for the CLI and tests.
--
-- The C-backend functions are re-exported for backwards compatibility
-- with existing test files that import them directly; new code should
-- prefer 'generate'/'compileToFile'.
module GDFunc.CodeGen
    ( -- * Target selection
      Target(..)
    , generate
    , compileToFile
    , defaultExtensionFor
      -- * Legacy C-backend API (re-exports)
    , C.generateC
    , C.compileToCFile
    , C.CompileOptions(..)
    , C.defaultOptions
    ) where

import qualified GDFunc.CodeGen.C as C
import qualified GDFunc.CodeGen.GDScript as GDScript
import GDFunc.Parser (Module)

-- | Selectable compile target.
data Target = TargetC | TargetGDScript
    deriving stock (Eq, Show)

generate :: Target -> Module -> String
generate TargetC        = C.generateC
generate TargetGDScript = GDScript.generateGDScript

compileToFile :: Target -> FilePath -> Module -> IO ()
compileToFile TargetC path m =
    C.compileToCFile (C.defaultOptions { C.optOutputFile = path }) m
compileToFile TargetGDScript path m =
    GDScript.compileToGDScriptFile path m

defaultExtensionFor :: Target -> String
defaultExtensionFor TargetC        = ".c"
defaultExtensionFor TargetGDScript = ".gd"
