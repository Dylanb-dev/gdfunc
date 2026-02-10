{-# LANGUAGE LambdaCase #-}

module GDFunc.Parser
    ( Expr(..)
    , Type(..)
    , Pattern(..)
    , Declaration(..)
    , LetBinding(..)
    , Module(..)
    , Import(..)
    , parseModule
    , parseExpr
    , parseType
    , Parser
    , ParseError(..)
    ) where

import GDFunc.Scanner
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative ((<|>), many, some, optional)
import Data.Maybe (fromMaybe)
import Control.Monad (when, void)
import Data.Char (isUpper, isLower)


-- AST Definitions

data Module = Module
    { moduleName :: [String]
    , moduleExposing :: Maybe [String]
    , moduleImports :: [Import]
    , moduleDeclarations :: [Declaration]
    } deriving (Show, Eq)

data Import = Import
    { importName :: [String]
    , importAlias :: Maybe String
    , importExposing :: Maybe [String]
    } deriving (Show, Eq)

data Declaration
    = TypeAnnotation String Type
    | FunctionDecl String [Pattern] Expr
    | TypeDecl String [String] [(String, [Type])]  -- type Name vars = Constructor types | ...
    | SharedTypeDecl String [String] [(String, [Type])]  -- type shared Name vars = ...
    | TypeAlias String [String] Type
    | SharedTypeAlias String [String] Type  -- type shared alias Name = ...
    deriving (Show, Eq)

data Type
    = TVar String              -- a, b, comparable
    | TBorrowed Type           -- &Type (borrowed/non-consuming)
    | TShared Type             -- shared Type (can be freely copied)
    | TCon String [Type]       -- List a, Maybe Int
    | TArrow Type Type         -- a -> b
    | TLinearArrow Type Type   -- a -o b
    | TTuple [Type]            -- (a, b, c)
    | TRecord [(String, Type)] (Maybe String)  -- { x : Int } or { r | x : Int }
    deriving (Show, Eq)

data Expr
    = EVar String              -- variable reference
    | EBorrow Expr             -- &expr (borrow expression)
    | EInt Int
    | EFloat Double
    | EChar Char
    | EString String
    | EList [Expr]
    | ETuple [Expr]
    | ERecord [(String, Expr)]
    | ERecordUpdate String [(String, Expr)]
    | EIf Expr Expr Expr
    | ECase Bool Expr [(Pattern, Expr)]  -- Bool is True for case!
    | ELet [LetBinding] Expr
    | ELambda [Pattern] Expr
    | EApp Expr Expr           -- function application
    | EBinOp String Expr Expr  -- binary operators
    | EFieldAccess Expr String -- record.field
    | EParens Expr
    deriving (Show, Eq)

data Pattern
    = PVar String              -- x
    | PBorrow Pattern          -- &pattern (borrowed pattern)
    | PWildcard                -- _
    | PInt Int
    | PFloat Double
    | PChar Char
    | PString String
    | PCons Pattern Pattern    -- x :: xs
    | PList [Pattern]
    | PTuple [Pattern]
    | PRecord [String]
    | PCtor String [Pattern]   -- Constructor patterns
    | PAs String Pattern       -- x as pattern
    | PParens Pattern
    deriving (Show, Eq)

data LetBinding
    = LetAnnotation String Type
    | LetDef String [Pattern] Expr
    | LetDestructure Pattern Expr
    deriving (Show, Eq)

-- Parser State and Error

data ParserState = ParserState
    { tokens :: [Token]
    , currentPos :: Int
    } deriving (Show)

data ParseError = ParseError
    { errorMessage :: String
    , errorPosition :: Position
    } deriving (Show, Eq)

-- Add these instances:
instance Semigroup ParseError where
    e1 <> _ = e1  -- Keep the first error

instance Monoid ParseError where
    mempty = ParseError "" (Position 0 0)

type Parser a = ExceptT ParseError (State ParserState) a

-- Main parsing functions

parseModule :: [Token] -> Either ParseError Module
parseModule toks = evalState (runExceptT moduleParser) initialState
  where
    initialState = ParserState
        { tokens = filter (not . isSkippable) toks
        , currentPos = 0
        }
    -- Only skip comments, keep newlines!
    isSkippable tok = case tokenType tok of
        COMMENT _ -> True
        _ -> False

skipNewlines :: Parser ()
skipNewlines = do
    tok <- peek
    case tok of
        Just t | tokenType t == NEWLINE -> do
            token NEWLINE
            skipNewlines
        _ -> return ()

parseExpr :: [Token] -> Either ParseError Expr
parseExpr toks = evalState (runExceptT expression) initialState
  where
    initialState = ParserState
        { tokens = filter (not . isSkippable) toks
        , currentPos = 0
        }
    isSkippable tok = tokenType tok `elem` [NEWLINE, COMMENT ""]

parseType :: [Token] -> Either ParseError Type
parseType toks = evalState (runExceptT typeAnnotation) initialState
  where
    initialState = ParserState
        { tokens = filter (not . isSkippable) toks
        , currentPos = 0
        }
    isSkippable tok = tokenType tok `elem` [NEWLINE, COMMENT ""]

-- Parser Combinators

satisfy :: (TokenType -> Bool) -> Parser Token
satisfy predicate = do
    st <- get
    if currentPos st >= length (tokens st)
        then throwError $ ParseError "Unexpected end of input" (Position 0 0)
        else do
            let tok = tokens st !! currentPos st
            if predicate (tokenType tok)
                then do
                    put st { currentPos = currentPos st + 1 }
                    return tok
                else throwError $ ParseError
                    ("Unexpected token: " ++ show (tokenType tok))
                    (position tok)

token :: TokenType -> Parser Token
token expected = satisfy (== expected)

identifier :: Parser String
identifier = do
    tok <- satisfy $ \case
        IDENTIFIER _ -> True
        _ -> False
    case tokenType tok of
        IDENTIFIER name -> return name
        _ -> throwError $ ParseError "Expected identifier" (position tok)

-- Parse an uppercase identifier (constructor)
upperIdentifier :: Parser String
upperIdentifier = do
    tok <- satisfy $ \case
        IDENTIFIER (c:_) -> isUpper c
        _ -> False
    case tokenType tok of
        IDENTIFIER name -> return name
        _ -> throwError $ ParseError "Expected uppercase identifier" (position tok)

-- Parse a lowercase identifier (variable)
lowerIdentifier :: Parser String
lowerIdentifier = do
    tok <- satisfy $ \case
        IDENTIFIER (c:_) -> isLower c
        _ -> False
    case tokenType tok of
        IDENTIFIER name -> return name
        _ -> throwError $ ParseError "Expected lowercase identifier" (position tok)

linearIdentifier :: Parser String
linearIdentifier = do
    tok <- satisfy $ \case
        LINEAR_IDENT _ -> True
        _ -> False
    case tokenType tok of
        LINEAR_IDENT name -> return name
        _ -> throwError $ ParseError "Expected linear identifier" (position tok)

peek :: Parser (Maybe Token)
peek = do
    st <- get
    if currentPos st >= length (tokens st)
        then return Nothing
        else return $ Just (tokens st !! currentPos st)

check :: TokenType -> Parser Bool
check expected = do
    p <- peek
    return $ case p of
        Just tok -> tokenType tok == expected
        Nothing -> False

consume :: TokenType -> String -> Parser Token
consume expected msg = do
    found <- check expected
    if found
        then token expected
        else do
            p <- peek
            let pos = maybe (Position 0 0) position p
            throwError $ ParseError msg pos

-- Module Parser

moduleParser :: Parser Module
moduleParser = do
    skipNewlines
    token MODULE
    name <- modulePath
    exp <- optional exposingClause
    skipNewlines 
    imports <- many importDecl
    skipNewlines
    decls <- manyDecls
    token EOF
    return $ Module name exp imports decls

-- Parse declarations until EOF
manyDecls :: Parser [Declaration]
manyDecls = do
    skipNewlines
    tok <- peek
    case fmap tokenType tok of
        Just EOF -> return []
        Nothing -> return []
        Just _ -> do
            -- Try to parse a declaration
            st <- get
            result <- catchError
                (do decl <- declaration
                    return (Just decl))
                (\_ -> do
                    put st
                    return Nothing)
            case result of
                Nothing -> return []  -- No more declarations
                Just decl -> do
                    skipNewlines
                    rest <- manyDecls
                    return (decl : rest)

modulePath :: Parser [String]
modulePath = do
    first <- identifier
    rest <- many $ do
        token DOT
        identifier
    return (first : rest)

exposingClause :: Parser [String]
exposingClause = do
    token EXPOSING
    token LEFT_PAREN
    exposed <- exposingList
    token RIGHT_PAREN
    return exposed

exposingList :: Parser [String]
exposingList = do
    first <- identifier
    rest <- many $ do
        token COMMA
        identifier
    return (first : rest)

importDecl :: Parser Import
importDecl = do
    token IMPORT
    name <- modulePath
    alias <- optional $ do
        token AS
        identifier
    exp <- optional exposingClause
    return $ Import name alias exp

-- Declaration Parser

declaration :: Parser Declaration
declaration = try sharedTypeAliasDeclaration
    <|> try typeAliasDeclaration
    <|> try sharedTypeDeclaration
    <|> try typeDeclaration
    <|> try typeAnnotationDeclaration
    <|> functionDeclaration

typeDeclaration :: Parser Declaration
typeDeclaration = do
    token TYPE
    name <- identifier
    typeVars <- many (try identifier)  -- Wrap identifier with try for backtracking
    token EQUALS
    constructors <- constructorList
    return $ TypeDecl name typeVars constructors

sharedTypeDeclaration :: Parser Declaration
sharedTypeDeclaration = do
    token TYPE
    token SHARED
    name <- identifier
    typeVars <- many (try identifier)  -- Wrap identifier with try for backtracking
    token EQUALS
    constructors <- constructorList
    return $ SharedTypeDecl name typeVars constructors

constructorList :: Parser [(String, [Type])]
constructorList = do
    first <- constructor
    rest <- many $ do
        satisfy (\t -> t == PIPE || t == NEWLINE)
        constructor
    return (first : rest)

constructor :: Parser (String, [Type])
constructor = do
    name <- identifier
    types <- many typeAtom
    return (name, types)

typeAliasDeclaration :: Parser Declaration
typeAliasDeclaration = do
    token TYPE
    token ALIAS
    name <- identifier
    typeVars <- many (try identifier)  -- Wrap identifier with try for backtracking
    token EQUALS
    typ <- typeAnnotation
    return $ TypeAlias name typeVars typ

sharedTypeAliasDeclaration :: Parser Declaration
sharedTypeAliasDeclaration = do
    token TYPE
    token SHARED
    token ALIAS
    name <- identifier
    typeVars <- many (try identifier)  -- Wrap identifier with try for backtracking
    token EQUALS
    typ <- typeAnnotation
    return $ SharedTypeAlias name typeVars typ

-- Parse a standalone type annotation declaration
typeAnnotationDeclaration :: Parser Declaration
typeAnnotationDeclaration = do
    name <- lowerIdentifier  -- Use lowercase identifier for functions
    token COLON
    typ <- typeAnnotation
    return $ TypeAnnotation name typ

-- Parse a standalone function declaration (following Elm's approach)
functionDeclaration :: Parser Declaration
functionDeclaration = do
    name <- lowerIdentifier
    (patterns, body) <- parsePatternsAndBody []
    return $ FunctionDecl name patterns body
  where
    -- Try to parse a pattern and recurse, OR parse equals and body
    parsePatternsAndBody revPatterns = do
        -- Save state before attempting
        st <- get
        -- Try to parse a pattern
        result <- catchError
            (do pat <- pattern
                return (Just pat))
            (\_ -> do
                -- Pattern failed, restore state
                put st
                return Nothing)
        case result of
            Just pat -> parsePatternsAndBody (pat : revPatterns)
            Nothing -> do
                -- No more patterns, parse equals and body
                token EQUALS
                body <- expression
                return (reverse revPatterns, body)

-- Type Parser

typeAnnotation :: Parser Type
typeAnnotation = typeArrow

-- Handles 'a -> b' and 'a -o b' (Right Associative)
typeArrow :: Parser Type
typeArrow = do
    left <- typeApplication
    p <- peek
    case fmap tokenType p of
        Just ARROW -> do
            void $ token ARROW
            right <- typeArrow
            return $ TArrow left right
        Just LINEAR_ARROW -> do
            void $ token LINEAR_ARROW
            right <- typeArrow
            return $ TLinearArrow left right
        _ -> return left

-- Handles 'List Int' or 'Maybe (List a)' or '&List Int' or 'shared List Int'
typeApplication :: Parser Type
typeApplication = do
    -- Check for & prefix (borrowed)
    isBorrowed <- check AMPERSAND
    if isBorrowed
        then do
            token AMPERSAND
            innerType <- typeApplication
            return (TBorrowed innerType)
        else do
            -- Check for shared prefix
            isShared <- check SHARED
            if isShared
                then do
                    token SHARED
                    innerType <- typeApplication
                    return (TShared innerType)
                else do
                    -- Parse one atom, then collect more atoms carefully
                    first <- typeAtom
                    rest <- collectTypeAtoms []
                    case first : rest of
                        [t] -> return t
                        (TCon name [] : args) -> return (TCon name args)
                        (TVar name : args) -> return (TCon name args)
                        _ -> throwError $ ParseError "Invalid type application" (Position 0 0)
  where
    -- Collect type atoms, stopping at tokens that end a type
    collectTypeAtoms acc = do
        tok <- peek
        case fmap tokenType tok of
            -- Stop at tokens that can't be part of a type application
            Just ARROW -> return (reverse acc)
            Just LINEAR_ARROW -> return (reverse acc)
            Just EQUALS -> return (reverse acc)
            Just PIPE -> return (reverse acc)
            Just COMMA -> return (reverse acc)
            Just RIGHT_PAREN -> return (reverse acc)
            Just RIGHT_BRACE -> return (reverse acc)
            Just RIGHT_BRACKET -> return (reverse acc)
            Just COLON -> return (reverse acc)
            Just NEWLINE -> return (reverse acc)
            Just EOF -> return (reverse acc)
            -- Try to parse another type atom
            Just _ -> do
                st <- get
                result <- catchError
                    (do atom <- typeAtom
                        return (Just atom))
                    (\_ -> do
                        put st
                        return Nothing)
                case result of
                    Just atom -> collectTypeAtoms (atom : acc)
                    Nothing -> return (reverse acc)
            Nothing -> return (reverse acc)

typeAtom :: Parser Type
typeAtom = typeVarOrCon
    <|> typeTuple
    <|> typeRecord
    <|> typeParens

typeVarOrCon :: Parser Type
typeVarOrCon = do
    tok <- peek
    case tok of
        Just t -> case tokenType t of
            IDENTIFIER name -> do
                _ <- token (IDENTIFIER name)
                let baseType = if isUpper (head name)
                              then TCon name []
                              else TVar name
                return baseType

            _ -> throwError $ ParseError "Expected type" (position t)
        Nothing -> throwError $ ParseError "Unexpected end" (Position 0 0)

-- Rest of the type parsing functions remain the same...
typeTuple :: Parser Type
typeTuple = do
    token LEFT_PAREN
    types <- typeList
    token RIGHT_PAREN
    case types of
        [] -> return $ TCon "()" []
        [t] -> return t
        _ -> return $ TTuple types

typeList :: Parser [Type]
typeList = do
    isEmpty <- check RIGHT_PAREN
    if isEmpty
        then return []
        else do
            first <- typeAnnotation
            rest <- many $ do
                token COMMA
                typeAnnotation
            return (first : rest)

typeRecord :: Parser Type
typeRecord = do
    token LEFT_BRACE
    extension <- optional $ try $ do
        name <- identifier
        token PIPE
        return name
    fields <- recordTypeFields
    token RIGHT_BRACE
    return $ TRecord fields extension

recordTypeFields :: Parser [(String, Type)]
recordTypeFields = do
    isEmpty <- check RIGHT_BRACE
    if isEmpty
        then return []
        else do
            first <- recordTypeField
            rest <- many $ do
                token COMMA
                recordTypeField
            return (first : rest)

recordTypeField :: Parser (String, Type)
recordTypeField = do
    name <- identifier
    token COLON
    typ <- typeAnnotation
    return (name, typ)

typeParens :: Parser Type
typeParens = do
    token LEFT_PAREN
    typ <- typeAnnotation
    token RIGHT_PAREN
    return typ

-- Expression Parser

expression :: Parser Expr
expression = ifExpression
    <|> caseExpression
    <|> letExpression
    <|> lambdaExpression
    <|> binaryExpression

ifExpression :: Parser Expr
ifExpression = do
    token IF
    condition <- expression
    token THEN
    thenBranch <- expression
    token ELSE
    elseBranch <- expression
    return $ EIf condition thenBranch elseBranch

caseExpression :: Parser Expr
caseExpression = do
    isLinear <- (token CASE_LINEAR >> return True)
        <|> (token CASE >> return False)
    scrutinee <- expression
    token OF
    branches <- many caseBranch
    return $ ECase isLinear scrutinee branches

caseBranch :: Parser (Pattern, Expr)
caseBranch = do
    pat <- pattern
    token ARROW
    expr <- expression
    return (pat, expr)

letExpression :: Parser Expr
letExpression = do
    token LET
    bindings <- many letBinding
    token IN
    body <- expression
    return $ ELet bindings body

letBinding :: Parser LetBinding
letBinding = try letAnnotationBinding
    <|> try letDestructureBinding
    <|> letDefBinding

letAnnotationBinding :: Parser LetBinding
letAnnotationBinding = do
    name <- identifier
    token COLON
    typ <- typeAnnotation
    return $ LetAnnotation name typ

letDefBinding :: Parser LetBinding
letDefBinding = do
    name <- identifier
    patterns <- many pattern
    token EQUALS
    expr <- expression
    return $ LetDef name patterns expr

letDestructureBinding :: Parser LetBinding
letDestructureBinding = do
    pat <- pattern
    token EQUALS
    expr <- expression
    return $ LetDestructure pat expr

lambdaExpression :: Parser Expr
lambdaExpression = do
    token BACKSLASH
    patterns <- some pattern
    token ARROW
    body <- expression
    return $ ELambda patterns body

binaryExpression :: Parser Expr
binaryExpression = do
    left <- applicationExpression
    maybeContinue left
  where
    maybeContinue left = do
        tok <- peek
        case tok of
            Just t | isOperatorToken (tokenType t) -> do
                op <- operator
                right <- applicationExpression
                maybeContinue (EBinOp op left right)
            _ -> return left

isOperatorToken :: TokenType -> Bool
isOperatorToken t = t `elem`
    [ PLUS, MINUS, MULTIPLY, DIVIDE, EQUALS, LESS_EQUAL, GREATER_EQUAL
    , LESS_THAN, GREATER_THAN, DOUBLE_EQUALS, NOT_EQUALS
    , APPEND, CONS, PIPE, AND, OR
    ]

operator :: Parser String
operator = do
    tok <- satisfy isOperatorToken
    return $ case tokenType tok of
        PLUS -> "+"
        MINUS -> "-"
        MULTIPLY -> "*"
        DIVIDE -> "/"
        EQUALS -> "="
        LESS_EQUAL -> "<="
        PLUS -> "+"
        MINUS -> "-"
        MULTIPLY -> "*"
        DIVIDE -> "/"
        APPEND -> "++"
        CONS -> "::"
        PIPE -> "|>"
        COMPOSE_LEFT -> "<|"
        COMPOSE_RIGHT -> ">>"
        COMPOSE -> "<<"
        DOUBLE_EQUALS -> "=="
        NOT_EQUALS -> "/="
        LESS_THAN -> "<"
        GREATER_THAN -> ">"
        LESS_EQUAL -> "<="
        GREATER_EQUAL -> ">="
        AND -> "&&"
        OR -> "||"
        _ -> ""

applicationExpression :: Parser Expr
applicationExpression = do
    first <- accessExpression
    rest <- many (try parseNextIfNotDecl)
    case first : rest of
        [e] -> return e
        (f:args) -> return $ foldl EApp f args
        [] -> throwError $ ParseError "Empty application" (Position 0 0)
  where
    parseNextIfNotDecl = do
        st <- get
        let remaining = drop (currentPos st) (tokens st)
        case remaining of
            -- Stop if we see "name =" or "name :"
            (t1:t2:_) | isIdentifierToken (tokenType t1) && 
                        (tokenType t2 == EQUALS || tokenType t2 == COLON) ->
                throwError $ ParseError "Declaration boundary" (position t1)
            _ -> accessExpression
        
    isIdentifierToken (IDENTIFIER _) = True
    isIdentifierToken (LINEAR_IDENT _) = True
    isIdentifierToken _ = False

accessExpression :: Parser Expr
accessExpression = do
    expr <- primaryExpression
    fields <- many $ do
        token DOT
        identifier
    return $ foldl EFieldAccess expr fields

primaryExpression :: Parser Expr
primaryExpression = borrowExpression
    <|> literalExpression
    <|> variableExpression
    <|> listExpression
    <|> recordExpression
    <|> parensExpression

borrowExpression :: Parser Expr
borrowExpression = do
    token AMPERSAND
    expr <- primaryExpression
    return $ EBorrow expr

literalExpression :: Parser Expr
literalExpression = do
    tok <- satisfy isLiteral
    case tokenType tok of
        NUMBER n -> return $ EInt n
        FLOAT f -> return $ EFloat f
        CHAR c -> return $ EChar c
        STRING s -> return $ EString s
        _ -> throwError $ ParseError "Expected literal" (position tok)
  where
    isLiteral t = case t of
        NUMBER _ -> True
        FLOAT _ -> True
        CHAR _ -> True
        STRING _ -> True
        _ -> False

variableExpression :: Parser Expr
variableExpression = do
    tok <- peek
    case tok of
        Just t -> case tokenType t of
            IDENTIFIER name -> do
                token (IDENTIFIER name)
                return $ EVar name
            _ -> throwError $ ParseError "Expected variable" (position t)
        Nothing -> throwError $ ParseError "Unexpected end" (Position 0 0)

listExpression :: Parser Expr
listExpression = do
    token LEFT_BRACKET
    elements <- expressionList
    token RIGHT_BRACKET
    return $ EList elements

expressionList :: Parser [Expr]
expressionList = do
    isEmpty <- check RIGHT_BRACKET
    if isEmpty
        then return []
        else do
            first <- expression
            rest <- many $ do
                token COMMA
                expression
            return (first : rest)

recordExpression :: Parser Expr
recordExpression = do
    token LEFT_BRACE
    isUpdate <- optional $ try $ do
        name <- identifier
        token PIPE
        return name
    fields <- recordFields
    token RIGHT_BRACE
    case isUpdate of
        Nothing -> return $ ERecord fields
        Just name -> return $ ERecordUpdate name fields

recordFields :: Parser [(String, Expr)]
recordFields = do
    isEmpty <- check RIGHT_BRACE
    if isEmpty
        then return []
        else do
            first <- recordField
            rest <- many $ do
                token COMMA
                recordField
            return (first : rest)

recordField :: Parser (String, Expr)
recordField = do
    name <- identifier
    token EQUALS
    expr <- expression
    return (name, expr)

parensExpression :: Parser Expr
parensExpression = do
    token LEFT_PAREN
    exprs <- expressionList
    token RIGHT_PAREN
    case exprs of
        [] -> return $ EVar "()"
        [e] -> return $ EParens e
        _ -> return $ ETuple exprs

-- Pattern Parser

pattern :: Parser Pattern
pattern = do
    -- Check if we're at a token that could start a pattern
    tok <- peek
    case fmap tokenType tok of
        Just EQUALS -> throwError $ ParseError "Cannot start pattern with =" (Position 0 0)
        Just COLON -> throwError $ ParseError "Cannot start pattern with :" (Position 0 0)
        Just ARROW -> throwError $ ParseError "Cannot start pattern with ->" (Position 0 0)
        _ -> consPattern

consPattern :: Parser Pattern
consPattern = do
    left <- patternAtom
    -- Check if next token is CONS before trying to parse it
    isCons <- check CONS
    if isCons
        then do
            token CONS
            right <- consPattern
            return $ PCons left right
        else return left

patternAtom :: Parser Pattern
patternAtom = try borrowPattern
    <|> try wildcardPattern
    <|> try literalPattern
    <|> try constructorPattern  -- Try constructor before variable (both use identifiers)
    <|> try variablePattern
    <|> try listPattern
    <|> try tuplePattern
    <|> try parensPattern

borrowPattern :: Parser Pattern
borrowPattern = do
    token AMPERSAND
    pat <- patternAtom
    return $ PBorrow pat

wildcardPattern :: Parser Pattern
wildcardPattern = do
    tok <- satisfy (\t -> case t of
        IDENTIFIER "_" -> True
        _ -> False)
    return PWildcard

literalPattern :: Parser Pattern
literalPattern = do
    tok <- satisfy isLiteral
    case tokenType tok of
        NUMBER n -> return $ PInt n
        FLOAT f -> return $ PFloat f
        CHAR c -> return $ PChar c
        STRING s -> return $ PString s
        _ -> throwError $ ParseError "Expected literal pattern" (position tok)
  where
    isLiteral t = case t of
        NUMBER _ -> True
        FLOAT _ -> True
        CHAR _ -> True
        STRING _ -> True
        _ -> False

variablePattern :: Parser Pattern
variablePattern = do
    name <- lowerIdentifier  -- Only match lowercase identifiers (variables)
    return $ PVar name

listPattern :: Parser Pattern
listPattern = do
    token LEFT_BRACKET
    patterns <- patternList
    token RIGHT_BRACKET
    return $ PList patterns

patternList :: Parser [Pattern]
patternList = do
    isEmpty <- check RIGHT_BRACKET
    if isEmpty
        then return []
        else do
            first <- pattern
            rest <- many $ do
                token COMMA
                pattern
            return (first : rest)

tuplePattern :: Parser Pattern
tuplePattern = do
    token LEFT_PAREN
    patterns <- patternList
    token RIGHT_PAREN
    case patterns of
        [p] -> return p
        _ -> return $ PTuple patterns

constructorPattern :: Parser Pattern
constructorPattern = do
    name <- upperIdentifier  -- Only match uppercase identifiers (constructors)
    args <- many patternAtom
    return $ PCtor name args

parensPattern :: Parser Pattern
parensPattern = do
    token LEFT_PAREN
    pat <- pattern
    token RIGHT_PAREN
    return $ PParens pat

-- Helper: try combinator
try :: Parser a -> Parser a
try p = do
    st <- get
    result <- catchError (Right <$> p) (return . Left)
    case result of
        Right val -> return val
        Left err -> do
            put st  -- Reset state on failure
            throwError err