{-# LANGUAGE LambdaCase #-}

module Main where

import GDFunc.Scanner
import GDFunc.Parser
import GDFunc.Pretty
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Control.Monad (when)
import qualified Data.Text.IO as TIO
import Repl (runRepl)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "GDFunc Compiler v0.1.0"
            putStrLn "Usage:"
            putStrLn "  gdfunc <file.elm>    - Compile a file"
            putStrLn "  gdfunc repl          - Start REPL"
            putStrLn "  gdfunc --help        - Show this help"
        
        ["repl"] -> runRepl
        
        ["--help"] -> do
            putStrLn "GDFunc - A functional language with linear types"
            putStrLn ""
            putStrLn "Usage: gdfunc [OPTIONS] [FILE]"
            putStrLn ""
            putStrLn "Options:"
            putStrLn "  repl              Start interactive REPL"
            putStrLn "  --parse FILE      Parse and show AST"
            putStrLn "  --pretty FILE     Parse and pretty-print"
            putStrLn "  --help            Show this help"
        
        ["--parse", filename] -> parseFile filename
        
        ["--pretty", filename] -> prettyFile filename
        
        [filename] -> compileFile filename
        
        _ -> do
            putStrLn "Invalid arguments. Use --help for usage."
            exitFailure

compileFile :: FilePath -> IO ()
compileFile path = do
    putStrLn $ "Compiling: " ++ path
    content <- readFile path
    
    -- Scan
    tokens <- case scanTokens content of
        Left err -> do
            putStrLn $ "Scan error: " ++ err
            exitFailure
        Right toks -> return toks
    
    putStrLn $ "✓ Scanned " ++ show (length tokens) ++ " tokens"
    
    -- Parse
    ast <- case parseModule tokens of
        Left err -> do
            putStrLn $ "Parse error: " ++ show err
            exitFailure
        Right module' -> return module'
    
    putStrLn "✓ Parsed successfully"
    
    -- Pretty print
    putStrLn "\n--- Pretty Printed ---"
    putStrLn $ prettyModule ast
    
    putStrLn "\n✓ Compilation complete"
    exitSuccess

parseFile :: FilePath -> IO ()
parseFile path = do
    content <- readFile path
    
    tokens <- case scanTokens content of
        Left err -> do
            putStrLn $ "Scan error: " ++ err
            exitFailure
        Right toks -> return toks
    
    case parseModule tokens of
        Left err -> do
            putStrLn $ "Parse error: " ++ show err
            exitFailure
        Right ast -> do
            putStrLn $ show ast
            exitSuccess

prettyFile :: FilePath -> IO ()
prettyFile path = do
    content <- readFile path
    
    tokens <- case scanTokens content of
        Left err -> do
            putStrLn $ "Scan error: " ++ err
            exitFailure
        Right toks -> return toks
    
    case parseModule tokens of
        Left err -> do
            putStrLn $ "Parse error: " ++ show err
            exitFailure
        Right ast -> do
            putStrLn $ prettyModule ast
            exitSuccess