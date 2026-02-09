{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module GDFunc.WebServer
    ( runWebServer
    , ServerConfig(..)
    , defaultConfig
    , CompileRequest(..)      
    , CompileResponse(..)     
    , RunResponse(..)         
    , compileEndpoint
    , compileGDFunc
    , compileAndRun        
    , runEndpoint             
    ) where

import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.RequestLogger
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import GHC.Generics
import System.Process
import System.Exit (ExitCode(..))
import System.IO
import System.IO.Temp
import System.FilePath
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import System.Directory (listDirectory, doesFileExist)
import Data.Char (toUpper)

import GDFunc.Scanner
import GDFunc.Parser
import GDFunc.TypeChecker
import GDFunc.CodeGen

-- Server configuration
data ServerConfig = ServerConfig
    { serverPort :: Int
    , staticDir :: FilePath
    , examplesDir :: FilePath
    } deriving (Show)

defaultConfig :: ServerConfig
defaultConfig = ServerConfig
    { serverPort = 3000
    , staticDir = "static"
    , examplesDir = "examples"
    }

-- API request/response types
data CompileRequest = CompileRequest
    { sourceCode :: String
    } deriving (Generic, Show)

instance FromJSON CompileRequest
instance ToJSON CompileRequest

data CompileResponse = CompileResponse
    { success :: Bool
    , cCode :: Maybe String
    , errorMessage :: Maybe String
    , scanOutput :: String
    , parseOutput :: String
    , typeCheckOutput :: String
    } deriving (Generic, Show)

instance FromJSON CompileResponse
instance ToJSON CompileResponse

data RunResponse = RunResponse
    { runCompileSuccess :: Bool
    , runSuccess :: Bool
    , runCCode :: Maybe String
    , runStdout :: String
    , runStderr :: String
    , runErrorMessage :: Maybe String
    } deriving (Generic, Show)

instance FromJSON RunResponse
instance ToJSON RunResponse

data ExampleFile = ExampleFile
    { fileName :: String
    , displayName :: String
    } deriving (Generic, Show)

instance FromJSON ExampleFile
instance ToJSON ExampleFile

data ExampleContent = ExampleContent
    { exampleSuccess :: Bool
    , exampleContent :: Maybe String
    , exampleError :: Maybe String
    } deriving (Generic, Show)

instance FromJSON ExampleContent
instance ToJSON ExampleContent

-- Start web server
runWebServer :: ServerConfig -> IO ()
runWebServer config = do
    putStrLn $ "Starting GDFunc web server on http://localhost:" ++ show (serverPort config)
    scotty (serverPort config) $ do
        -- Middleware
        middleware logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase (staticDir config))

        -- Serve index page
        get "/" $ file (staticDir config </> "index.html")

        -- API endpoints
        post "/api/compile" compileEndpoint
        post "/api/runC" runCEndpoint
        post "/api/run" runEndpoint
        get "/api/examples" $ listExamplesEndpoint config
        get "/api/examples/:filename" $ getExampleEndpoint config

-- Compile endpoint - just generate C code
compileEndpoint :: ActionM ()
compileEndpoint = do
    req <- jsonData :: ActionM CompileRequest
    result <- liftIO $ compileGDFunc (sourceCode req)
    json result

-- Run endpoint - compile and execute
runEndpoint :: ActionM ()
runEndpoint = do
    req <- jsonData :: ActionM CompileRequest
    result <- liftIO $ compileAndRun (sourceCode req)
    json result

-- Run C endpoint - compile and execute
runCEndpoint :: ActionM ()
runCEndpoint = do
    req <- jsonData :: ActionM CompileRequest
    result <- liftIO $ compileAndRunC (sourceCode req)
    json result

-- Compile GDFunc source to C
compileGDFunc :: String -> IO CompileResponse
compileGDFunc source = do
    -- Scan
    case scanTokens source of
        Left err -> return $ CompileResponse
            { success = False
            , cCode = Nothing
            , errorMessage = Just err
            , scanOutput = "Scan failed: " ++ err
            , parseOutput = ""
            , typeCheckOutput = ""
            }
        Right tokens -> do
            let scanOut = "✓ Scanned " ++ show (length tokens) ++ " tokens"
            let filteredTokens = filter (not . isSkippable . tokenType) tokens
            
            -- Parse
            case parseModule filteredTokens of
                Left err -> return $ CompileResponse
                    { success = False
                    , cCode = Nothing
                    , errorMessage = Just (show err)
                    , scanOutput = scanOut
                    , parseOutput = "Parse failed: " ++ show err
                    , typeCheckOutput = ""
                    }
                Right ast -> do
                    let parseOut = "✓ Parsed successfully"
                    
                    -- Type check - just check it succeeds, don't use the result
                    case typeCheckModule ast of
                        Left err -> return $ CompileResponse
                            { success = False
                            , cCode = Nothing
                            , errorMessage = Just (show err)
                            , scanOutput = scanOut
                            , parseOutput = parseOut
                            , typeCheckOutput = "Type check failed: " ++ show err
                            }
                        Right _ -> do
                            let typeOut = "✓ Type checked"
                            
                            -- Generate C
                            let cSource = generateC ast
                            return $ CompileResponse
                                { success = True
                                , cCode = Just cSource
                                , errorMessage = Nothing
                                , scanOutput = scanOut
                                , parseOutput = parseOut
                                , typeCheckOutput = typeOut
                                }
-- Helper to check if token should be skipped
isSkippable :: TokenType -> Bool
isSkippable (COMMENT _) = True
isSkippable NEWLINE = True
isSkippable _ = False

-- Compile and run C code
compileAndRun :: String -> IO RunResponse
compileAndRun source = do
    -- First compile GDFunc to C
    compileResult@(CompileResponse success' cCode' errorMsg' _ _ _) <- compileGDFunc source
    
    if not success'
        then return $ RunResponse
            { runCompileSuccess = False
            , runSuccess = False
            , runCCode = cCode'
            , runStdout = ""
            , runStderr = ""
            , runErrorMessage = errorMsg'
            }
        else do
            -- Create temporary files
            withSystemTempDirectory "gdfunc" $ \tmpDir -> do
                let cFile = tmpDir </> "program.c"
                let exeFile = tmpDir </> "program"
                
                -- Write C code to file
                writeFile cFile (maybe "" id cCode')
                
                -- Compile with GCC
                (gccExit, gccStdout, gccStderr) <- readProcessWithExitCode 
                    "gcc" 
                    ["-O2", "-o", exeFile, cFile] 
                    ""
                
                case gccExit of
                    ExitFailure _ -> return $ RunResponse
                        { runCompileSuccess = False
                        , runSuccess = False
                        , runCCode = cCode'
                        , runStdout = gccStdout
                        , runStderr = "GCC compilation failed:\n" ++ gccStderr
                        , runErrorMessage = Just "C compilation failed"
                        }
                    ExitSuccess -> do
                        -- Run the compiled program
                        (runExit, runStdout', runStderr') <- readProcessWithExitCode exeFile [] ""
                        
                        return $ RunResponse
                            { runCompileSuccess = True
                            , runSuccess = runExit == ExitSuccess
                            , runCCode = cCode'
                            , runStdout = runStdout'
                            , runStderr = runStderr'
                            , runErrorMessage = if runExit == ExitSuccess 
                                then Nothing 
                                else Just "Program execution failed"
                            }


-- Compile and run C code only
compileAndRunC :: String -> IO RunResponse
compileAndRunC source = do
    -- Create temporary files
    withSystemTempDirectory "gdfunc" $ \tmpDir -> do
        let cFile = tmpDir </> "program.c"
        let exeFile = tmpDir </> "program"
        let cCode' = Just source
        -- Write C code to file
        writeFile cFile (maybe "" id cCode')

        -- Compile with GCC
        (gccExit, gccStdout, gccStderr) <- readProcessWithExitCode
            "gcc"
            ["-O2", "-o", exeFile, cFile]
            ""

        case gccExit of
            ExitFailure _ -> return $ RunResponse
                { runCompileSuccess = False
                , runSuccess = False
                , runCCode = cCode'
                , runStdout = gccStdout
                , runStderr = "GCC compilation failed:\n" ++ gccStderr
                , runErrorMessage = Just "C compilation failed"
                }
            ExitSuccess -> do
                -- Run the compiled program
                (runExit, runStdout', runStderr') <- readProcessWithExitCode exeFile [] ""

                return $ RunResponse
                    { runCompileSuccess = True
                    , runSuccess = runExit == ExitSuccess
                    , runCCode = cCode'
                    , runStdout = runStdout'
                    , runStderr = runStderr'
                    , runErrorMessage = if runExit == ExitSuccess
                        then Nothing
                        else Just "Program execution failed"
                    }

-- List available examples
listExamplesEndpoint :: ServerConfig -> ActionM ()
listExamplesEndpoint config = do
    files <- liftIO $ listExampleFiles (examplesDir config)
    json files

-- Get content of a specific example
getExampleEndpoint :: ServerConfig -> ActionM ()
getExampleEndpoint config = do
    filename <- param "filename"
    result <- liftIO $ loadExample (examplesDir config) filename
    json result

-- List all .gdfunc files in examples directory
listExampleFiles :: FilePath -> IO [ExampleFile]
listExampleFiles dir = do
    allFiles <- listDirectory dir
    let gdfuncFiles = filter (\f -> takeExtension f == ".gdfunc") allFiles
    return $ map makeExampleFile gdfuncFiles
  where
    makeExampleFile :: String -> ExampleFile
    makeExampleFile fname = ExampleFile
        { fileName = fname
        , displayName = makeDisplayName fname
        }

    makeDisplayName :: String -> String
    makeDisplayName fname =
        let baseName = takeBaseName fname
            -- Convert underscores to spaces and capitalize words
            withSpaces = map (\c -> if c == '_' then ' ' else c) baseName
            capitalized = unwords $ map capitalizeFirst $ words withSpaces
        in capitalized

    capitalizeFirst :: String -> String
    capitalizeFirst [] = []
    capitalizeFirst (c:cs) = toUpper c : cs

-- Load an example file
loadExample :: FilePath -> String -> IO ExampleContent
loadExample dir filename = do
    let filepath = dir </> filename
    exists <- doesFileExist filepath

    if not exists
        then return $ ExampleContent
            { exampleSuccess = False
            , exampleContent = Nothing
            , exampleError = Just "Example file not found"
            }
        else do
            content <- readFile filepath
            return $ ExampleContent
                { exampleSuccess = True
                , exampleContent = Just content
                , exampleError = Nothing
                }
