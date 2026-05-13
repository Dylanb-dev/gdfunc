-- | Golden tests for the code generators.
--
-- A golden test runs the compile pipeline on a fixed input and
-- compares the output against a frozen reference file. On the first
-- run for a given test name @hspec-golden@ writes the current output
-- to @.golden\/\<name\>\/golden@ and the test passes; on subsequent
-- runs it diffs against that file and fails on any byte difference.
--
-- This is load-bearing for the two-backend (C + GDScript) effort: it
-- catches accidental codegen drift when refactoring upstream stages.
module GDFunc.GoldenSpec (spec) where

import Test.Hspec
import Test.Hspec.Golden (defaultGolden)

import qualified GDFunc.Parser2 as Parser
import qualified GDFunc.CodeGen as CodeGen

spec :: Spec
spec = describe "Codegen golden output" $ do
    cOutput  <- runIO $ runPipeline CodeGen.TargetC        "examples/factorial.gdfunc"
    gdOutput <- runIO $ runPipeline CodeGen.TargetGDScript "examples/factorial.gdfunc"

    it "factorial.gdfunc -> C" $
        defaultGolden "factorial-c" cOutput
    it "factorial.gdfunc -> GDScript" $
        defaultGolden "factorial-gdscript" gdOutput

-- | Run the pipeline and return whatever it produces, capturing
-- parse errors as the golden output. We deliberately /don't/ throw:
-- the first run pins current behavior; subsequent runs fail loudly
-- when codegen or parser output drifts.
runPipeline :: CodeGen.Target -> FilePath -> IO String
runPipeline target path = do
    source <- readFile path
    case Parser.parseModule path source of
        Left err -> pure $ "PARSE_ERROR:\n" ++ err
        Right m  -> pure $ CodeGen.generate target m
