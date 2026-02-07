module Repl (runRepl) where

import GDFunc.Scanner
import GDFunc.Parser
import GDFunc.Pretty
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)

runRepl :: IO ()
runRepl = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "elm-linear> "
        case minput of
            Nothing -> return ()
            Just input
                | input == ":quit" || input == ":q" -> do
                    outputStrLn "Goodbye!"
                    return ()
                | input == ":help" || input == ":h" -> do
                    liftIO printHelp
                    loop
                | ":load " `isPrefixOf` input -> do
                    let filename = drop 6 input
                    liftIO $ loadFile filename
                    loop
                | ":type " `isPrefixOf` input -> do
                    let expr = drop 6 input
                    liftIO $ checkType expr
                    loop
                | ":parse " `isPrefixOf` input -> do
                    let expr = drop 7 input
                    liftIO $ parseExpression expr
                    loop
                | otherwise -> do
                    liftIO $ evalExpression input
                    loop

printHelp :: IO ()
printHelp = do
    putStrLn ""
    putStrLn "GDFunc REPL Commands:"
    putStrLn "  :help, :h          Show this help"
    putStrLn "  :quit, :q          Exit REPL"
    putStrLn "  :load <file>       Load a file"
    putStrLn "  :type <expr>       Show type of expression"
    putStrLn "  :parse <expr>      Parse and show AST"
    putStrLn "  <expr>             Evaluate expression"
    putStrLn ""

loadFile :: FilePath -> IO ()
loadFile path = do
    content <- readFile path
    putStrLn $ "Loading: " ++ path
    
    case scanTokens content of
        Left err -> putStrLn $ "Scan error: " ++ err
        Right tokens -> case parseModule tokens of
            Left err -> putStrLn $ "Parse error: " ++ show err
            Right ast -> do
                putStrLn "✓ Loaded successfully"
                putStrLn ""
                putStrLn $ prettyModule ast

checkType :: String -> IO ()
checkType expr = do
    case scanTokens expr of
        Left err -> putStrLn $ "Scan error: " ++ err
        Right tokens -> case parseExpr tokens of
            Left err -> putStrLn $ "Parse error: " ++ show err
            Right ast -> do
                -- TODO: Implement type inference
                putStrLn $ "Expression: " ++ prettyExpr ast
                putStrLn "Type inference not yet implemented"

parseExpression :: String -> IO ()
parseExpression expr = do
    case scanTokens expr of
        Left err -> putStrLn $ "Scan error: " ++ err
        Right tokens -> case parseExpr tokens of
            Left err -> putStrLn $ "Parse error: " ++ show err
            Right ast -> putStrLn $ show ast

evalExpression :: String -> IO ()
evalExpression expr = do
    case scanTokens expr of
        Left err -> putStrLn $ "Scan error: " ++ err
        Right tokens -> case parseExpr tokens of
            Left err -> putStrLn $ "Parse error: " ++ show err
            Right ast -> do
                -- TODO: Implement interpreter
                putStrLn $ "Parsed: " ++ prettyExpr ast
                putStrLn "Interpreter not yet implemented"