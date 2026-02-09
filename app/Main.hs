-- app/Main.hs (update)

module Main where

import GDFunc.Scanner
import GDFunc.Parser
import GDFunc.TypeChecker
import GDFunc.CodeGen
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Process (callCommand)
import System.IO

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "GDFunc Compiler v0.1.0"
            putStrLn "Usage:"
            putStrLn "  gdfunc <file.gd>           - Compile to C and executable"
            putStrLn "  gdfunc --emit-c <file.gd>  - Emit C code only"
            putStrLn "  gdfunc --help              - Show this help"
        
        ["--help"] -> showHelp
        
        ["--emit-c", filename] -> emitCOnly filename
        
        [filename] -> compileAndLink filename
        
        _ -> do
            putStrLn "Invalid arguments. Use --help for usage."
            exitFailure

showHelp :: IO ()
showHelp = do
    putStrLn "GDFunc - A functional language with linear types"
    putStrLn ""
    putStrLn "Usage: gdfunc [OPTIONS] <file>"
    putStrLn ""
    putStrLn "Options:"
    putStrLn "  --emit-c          Emit C code only"
    putStrLn "  --help            Show this help"
    putStrLn ""
    putStrLn "Examples:"
    putStrLn "  gdfunc program.gd              # Compile to executable"
    putStrLn "  gdfunc --emit-c program.gd     # Generate C code"

emitCOnly :: FilePath -> IO ()
emitCOnly path = do
    putStrLn $ "Compiling: " ++ path
    content <- readFile path
    
    -- Scan
    tokens <- case scanTokens content of
        Left err -> do
            putStrLn $ "Scan error: " ++ err
            exitFailure
        Right toks -> return toks
    
    putStrLn $ "✓ Scanned " ++ show (length tokens) ++ " tokens"
    
    -- Filter out comments and newlines before parsing
    let filteredTokens = filter (not . isSkippable . tokenType) tokens
    
    -- Parse
    ast <- case parseModule filteredTokens of
        Left err -> do
            putStrLn $ "Parse error: " ++ show err
            exitFailure
        Right module' -> return module'
    
    putStrLn "✓ Parsed successfully"
    
    -- Generate C
    let opts = defaultOptions { optOutputFile = replaceExtension path ".c" }
    compileToCFile opts ast
    
    putStrLn $ "✓ Generated C code: " ++ optOutputFile opts
    exitSuccess

compileAndLink :: FilePath -> IO ()
compileAndLink path = do
    -- First emit C
    emitCOnly path
    
    -- Then compile with GCC
    let cFile = replaceExtension path ".c"
    let exeFile = replaceExtension path ""
    
    putStrLn $ "Compiling C code with GCC..."
    callCommand $ "gcc -O2 -o " ++ exeFile ++ " " ++ cFile
    
    putStrLn $ "✓ Compiled successfully: " ++ exeFile
    exitSuccess

replaceExtension :: FilePath -> String -> FilePath
replaceExtension path ext = 
    let base = reverse . dropWhile (/= '.') . reverse $ path
    in if '.' `elem` path
        then init base ++ ext
        else path ++ ext

isSkippable :: TokenType -> Bool
isSkippable (COMMENT _) = True
isSkippable NEWLINE = True
isSkippable _ = False