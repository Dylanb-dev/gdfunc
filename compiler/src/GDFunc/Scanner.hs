{-# LANGUAGE LambdaCase #-}

module GDFunc.Scanner
    ( Token(..)
    , TokenType(..)
    , scanTokens
    , showToken
    , Position(..)
    ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Control.Monad.State
import Control.Monad.Except
import Control.Monad (when)


-- Position tracking for error messages
data Position = Position
    { line :: Int
    , column :: Int
    } deriving (Show, Eq)

-- Token with position information
data Token = Token
    { tokenType :: TokenType
    , lexeme :: String
    , position :: Position
    } deriving (Show, Eq)

-- All token types in GDFunc
data TokenType
    -- Keywords
    = MODULE
    | EXPOSING
    | IMPORT
    | AS
    | TYPE
    | ALIAS
    | IF
    | THEN
    | ELSE
    | CASE
    | CASE_LINEAR      -- case!
    | OF
    | LET
    | IN
    | WHERE
    
    -- Linear-specific keywords
    | LINEAR_ARROW     -- -o
    | SHARED           -- shared keyword
    | AMPERSAND        -- & for borrowing

    -- Literals
    | IDENTIFIER String
    | LINEAR_IDENT String  -- identifier!
    | NUMBER Int
    | FLOAT Double
    | STRING String
    | CHAR Char
    
    -- Operators
    | PLUS
    | MINUS
    | MULTIPLY
    | DIVIDE
    | POWER
    | APPEND          -- ++
    | CONS            -- ::
    | PIPE            -- |>
    | COMPOSE_LEFT    -- <|
    | COMPOSE_RIGHT   -- >>
    | COMPOSE         -- 
    | ARROW           -- ->
    | BACKSLASH       -- \
    | DOT
    | COMMA
    | COLON
    | DOUBLE_COLON    -- ::
    | BANG            -- !
    | EQUALS
    | DOUBLE_EQUALS   -- ==
    | NOT_EQUALS      -- /=
    | LESS_THAN
    | GREATER_THAN
    | LESS_EQUAL      -- <=
    | GREATER_EQUAL   -- >=
    | AND             -- &&
    | OR              -- ||
    
    -- Delimiters
    | LEFT_PAREN
    | RIGHT_PAREN
    | LEFT_BRACKET
    | RIGHT_BRACKET
    | LEFT_BRACE
    | RIGHT_BRACE
    
    -- Special
    | NEWLINE
    | INDENT Int      -- Track indentation level
    | DEDENT Int
    | EOF
    | COMMENT String
    
    deriving (Show, Eq)

-- Scanner state
data ScannerState = ScannerState
    { source :: String
    , current :: Int
    , start :: Int
    , pos :: Position
    , tokens :: [Token]
    } deriving (Show)

type Scanner = ExceptT String (State ScannerState)

-- Main scanning function
scanTokens :: String -> Either String [Token]
scanTokens src = 
    let initialState = ScannerState
            { source = src
            , current = 0
            , start = 0
            , pos = Position 1 1
            , tokens = []
            }
    in evalState (runExceptT scanTokens') initialState

scanTokens' :: Scanner [Token]
scanTokens' = do
    atEnd <- isAtEnd
    if atEnd
        then do
            addToken EOF
            st <- get
            return $ reverse (tokens st)
        else do
            modify $ \s -> s { start = current s }
            scanToken
            scanTokens'

-- Scan a single token
scanToken :: Scanner ()
scanToken = do
    c <- advance
    case c of
        '(' -> addToken LEFT_PAREN
        ')' -> addToken RIGHT_PAREN
        '[' -> addToken LEFT_BRACKET
        ']' -> addToken RIGHT_BRACKET
        '{' -> addToken LEFT_BRACE
        '}' -> addToken RIGHT_BRACE
        ',' -> addToken COMMA
        '.' -> addToken DOT
        '!' -> addToken BANG
        '^' -> addToken POWER
        '+' -> do
            match '+' >>= \case
                True -> addToken APPEND
                False -> addToken PLUS
        '*' -> addToken MULTIPLY
        '/' -> do
            match '/' >>= \case
                True -> lineComment
                False -> match '=' >>= \case
                    True -> addToken NOT_EQUALS
                    False -> addToken DIVIDE
        '=' -> do
            match '=' >>= \case
                True -> addToken DOUBLE_EQUALS
                False -> addToken EQUALS
        '<' -> do
            match '=' >>= \case
                True -> addToken LESS_EQUAL
                False -> match '|' >>= \case
                    True -> addToken COMPOSE_LEFT
                    False -> match '<' >>= \case
                        True -> addToken COMPOSE
                        False -> addToken LESS_THAN
        '>' -> do
            match '=' >>= \case
                True -> addToken GREATER_EQUAL
                False -> match '>' >>= \case
                    True -> addToken COMPOSE_RIGHT
                    False -> addToken GREATER_THAN
        '|' -> do
            match '>' >>= \case
                True -> addToken PIPE
                False -> match '|' >>= \case
                    True -> addToken OR
                    False -> throwError "Unexpected character: |"
        '&' -> do
            match '&' >>= \case
                True -> addToken AND
                False -> addToken AMPERSAND
        ':' -> do
            match ':' >>= \case
                True -> addToken CONS
                False -> addToken COLON
        '-' -> do
            match 'o' >>= \case
                True -> addToken LINEAR_ARROW
                False -> match '-' >>= \case
                    True -> lineComment
                    False -> match '>' >>= \case
                        True -> addToken ARROW
                        False -> addToken MINUS
        '\\' -> addToken BACKSLASH
        '"' -> stringLiteral
        '\'' -> charLiteral
        '\n' -> do
            addToken NEWLINE
            modify $ \s -> s { pos = (pos s) { line = line (pos s) + 1, column = 1 } }
        ' ' -> return ()
        '\t' -> return ()
        '\r' -> return ()
        _ | isDigit c -> number
          | isAlpha c || c == '_' -> identifier
          | otherwise -> throwError $ "Unexpected character: " ++ [c]

-- Scan a number
number :: Scanner ()
number = do
    advanceWhile isDigit
    
    -- Check for decimal point
    p <- peek
    p2 <- peekNext
    
    if p == Just '.' && maybe False isDigit p2
        then do
            advance  -- consume '.'
            advanceWhile isDigit
            st <- get
            let lexeme = take (current st - start st) (drop (start st) (source st))
            let value = read lexeme :: Double
            addTokenWithLiteral (FLOAT value)
        else do
            st <- get
            let lexeme = take (current st - start st) (drop (start st) (source st))
            let value = read lexeme :: Int
            addTokenWithLiteral (NUMBER value)

-- Scan an identifier or keyword
identifier :: Scanner ()
identifier = do
    advanceWhile (\c -> isAlphaNum c || c == '_' || c == '\'')
    
    st <- get
    let text = take (current st - start st) (drop (start st) (source st))
    
    -- Special handling for "case" - check if followed by !
    if text == "case"
        then do
            p <- peek
            if p == Just '!'
                then do
                    advance  -- consume the !
                    addTokenWithLiteral CASE_LINEAR
                else addTokenWithLiteral CASE
        else do
            -- Check other keywords first
            case lookup text keywords of
                Just kw -> addTokenWithLiteral kw
                Nothing -> do
                    -- Not a keyword, check for linear identifier (ends with !)
                    p <- peek
                    hasLinear <- if p == Just '!'
                        then do
                            advance
                            return True
                        else return False
                    
                    if hasLinear
                        then addTokenWithLiteral (LINEAR_IDENT text)
                        else addTokenWithLiteral (IDENTIFIER text)

-- Keywords map
keywords :: [(String, TokenType)]
keywords =
    [ ("module", MODULE)
    , ("exposing", EXPOSING)
    , ("import", IMPORT)
    , ("as", AS)
    , ("type", TYPE)
    , ("alias", ALIAS)
    , ("if", IF)
    , ("then", THEN)
    , ("else", ELSE)
    , ("case", CASE)
    , ("of", OF)
    , ("let", LET)
    , ("in", IN)
    , ("where", WHERE)
    , ("shared", SHARED)
    ]

-- Scan a string literal
stringLiteral :: Scanner ()
stringLiteral = do
    str <- stringHelper []
    addTokenWithLiteral (STRING str)
  where
    stringHelper acc = do
        atEnd <- isAtEnd
        if atEnd
            then throwError "Unterminated string"
            else do
                c <- advance
                case c of
                    '"' -> return (reverse acc)
                    '\\' -> do
                        escaped <- advance
                        let char = case escaped of
                                'n' -> '\n'
                                't' -> '\t'
                                'r' -> '\r'
                                '\\' -> '\\'
                                '"' -> '"'
                                _ -> escaped
                        stringHelper (char : acc)
                    '\n' -> do
                        modify $ \s -> s { pos = (pos s) { line = line (pos s) + 1, column = 1 } }
                        stringHelper (c : acc)
                    _ -> stringHelper (c : acc)

-- Scan a character literal
charLiteral :: Scanner ()
charLiteral = do
    atEnd <- isAtEnd
    when atEnd $ throwError "Unterminated character literal"
    
    c <- advance
    char <- case c of
        '\\' -> do
            escaped <- advance
            return $ case escaped of
                'n' -> '\n'
                't' -> '\t'
                'r' -> '\r'
                '\\' -> '\\'
                '\'' -> '\''
                _ -> escaped
        _ -> return c
    
    match '\'' >>= \case
        True -> addTokenWithLiteral (CHAR char)
        False -> throwError "Unterminated character literal"

-- Scan a line comment
lineComment :: Scanner ()
lineComment = do
    st <- get
    let commentStart = current st
    advanceWhile (/= '\n')
    st' <- get
    let comment = take (current st' - commentStart) (drop commentStart (source st'))
    addTokenWithLiteral (COMMENT comment)

-- Helper functions
advance :: Scanner Char
advance = do
    st <- get
    let c = source st !! current st
    put st { current = current st + 1, pos = (pos st) { column = column (pos st) + 1 } }
    return c

peek :: Scanner (Maybe Char)
peek = do
    st <- get
    if current st >= length (source st)
        then return Nothing
        else return $ Just (source st !! current st)

peekNext :: Scanner (Maybe Char)
peekNext = do
    st <- get
    if current st + 1 >= length (source st)
        then return Nothing
        else return $ Just (source st !! (current st + 1))

match :: Char -> Scanner Bool
match expected = do
    atEnd <- isAtEnd
    if atEnd
        then return False
        else do
            st <- get
            if source st !! current st == expected
                then do
                    advance
                    return True
                else return False

advanceWhile :: (Char -> Bool) -> Scanner ()
advanceWhile predicate = do
    p <- peek
    case p of
        Nothing -> return ()
        Just c -> when (predicate c) $ do
            advance
            advanceWhile predicate

isAtEnd :: Scanner Bool
isAtEnd = do
    st <- get
    return $ current st >= length (source st)

addToken :: TokenType -> Scanner ()
addToken tokenType = do
    st <- get
    let text = take (current st - start st) (drop (start st) (source st))
    let token = Token tokenType text (pos st)
    put st { tokens = token : tokens st }

addTokenWithLiteral :: TokenType -> Scanner ()
addTokenWithLiteral = addToken

-- Pretty print a token
showToken :: Token -> String
showToken (Token ttype lex (Position l c)) =
    show l ++ ":" ++ show c ++ " " ++ show ttype ++ " '" ++ lex ++ "'"