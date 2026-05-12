module Main (main) where

import qualified GDFunc.Scanner as Scanner
import qualified GDFunc.Parser as Parser
import qualified GDFunc.CodeGen as CodeGen

import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.Process (callCommand)
import System.FilePath ((-<.>), dropExtension)

data Options = Options
    { optInputFile :: FilePath
    , optTarget    :: CodeGen.Target
    , optOutput    :: Maybe FilePath
    , optCheckOnly :: Bool
    , optEmitOnly  :: Bool
    }

targetReader :: ReadM CodeGen.Target
targetReader = eitherReader $ \case
    "c"        -> Right CodeGen.TargetC
    "gdscript" -> Right CodeGen.TargetGDScript
    other      -> Left $ "unknown target: " ++ other ++ " (expected: c | gdscript)"

targetParser :: Parser CodeGen.Target
targetParser =
    option targetReader
        ( long "target"
       <> short 't'
       <> metavar "TARGET"
       <> value CodeGen.TargetC
       <> showDefaultWith targetName
       <> help "Compile target: c | gdscript"
        )
  where
    targetName CodeGen.TargetC        = "c"
    targetName CodeGen.TargetGDScript = "gdscript"

optionsParser :: Parser Options
optionsParser = Options
    <$> strArgument
        ( metavar "FILE"
       <> help "Input .gdfunc source file"
        )
    <*> targetParser
    <*> optional (strOption
        ( long "output"
       <> short 'o'
       <> metavar "FILE"
       <> help "Output path (derived from input if omitted)"
        ))
    <*> switch
        ( long "check"
       <> help "Type-check only; do not emit code"
        )
    <*> switch
        ( long "emit-only"
       <> help "Emit generated source but skip downstream toolchain (cc for C)"
        )

opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
    ( fullDesc
   <> progDesc "Compile a GDFunc source file to GDScript or C"
   <> header   "gdfunc - the GDFunc compiler"
    )

main :: IO ()
main = execParser opts >>= runCompile

runCompile :: Options -> IO ()
runCompile o = do
    let inFile = optInputFile o
    source <- readFile inFile

    tokens <- case Scanner.scanTokens source of
        Left err -> die ("Scan error: " ++ err)
        Right ts -> pure ts

    let filtered = filter (not . isSkippable . Scanner.tokenType) tokens
    ast <- case Parser.parseModule filtered of
        Left err -> die ("Parse error: " ++ show err)
        Right m  -> pure m

    if optCheckOnly o
        then putStrLn "OK (parse only; type-checking pass is wired in phase 2)"
        else do
            let target  = optTarget o
            let outPath = case optOutput o of
                    Just p  -> p
                    Nothing -> inFile -<.> CodeGen.defaultExtensionFor target
            CodeGen.compileToFile target outPath ast
            case target of
                CodeGen.TargetC | not (optEmitOnly o) -> do
                    let exe = dropExtension outPath
                    putStrLn $ "Linking " ++ exe ++ " with cc..."
                    callCommand $ "cc -O2 -o " ++ exe ++ " " ++ outPath
                _ -> pure ()

die :: String -> IO a
die msg = do
    hPutStrLn stderr msg
    exitFailure

isSkippable :: Scanner.TokenType -> Bool
isSkippable (Scanner.COMMENT _) = True
isSkippable Scanner.NEWLINE     = True
isSkippable _                   = False
