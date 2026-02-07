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
    | TypeAlias String [String] Type
    deriving (Show, Eq)

data Type
    = TVar String              -- a, b, comparable
    | TLinear Type             -- Type!
    | TCon String [Type]       -- List a, Maybe Int
    | TArrow Type Type         -- a -> b
    | TLinearArrow Type Type   -- a -o b
    | TTuple [Type]            -- (a, b, c)
    | TRecord [(String, Type)] (Maybe String)  -- { x : Int } or { r | x : Int }
    deriving (Show, Eq)

data Expr
    = EVar String              -- variable reference
    | ELinearVar String        -- variable! reference
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
    | PLinearVar String        -- x!
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
    isSkippable tok = tokenType tok `elem` [NEWLINE, COMMENT ""]

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
    token MODULE
    name <- modulePath
    exp <- optional exposingClause
    imports <- many importDecl
    decls <- many declaration
    token EOF
    return $ Module name exp imports decls

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
declaration = typeDeclaration
    <|> typeAliasDeclaration
    <|> try functionAnnotation
    <|> functionDeclaration

typeDeclaration :: Parser Declaration
typeDeclaration = do
    token TYPE
    name <- identifier
    typeVars <- many identifier
    token EQUALS
    constructors <- constructorList
    return $ TypeDecl name typeVars constructors

constructorList :: Parser [(String, [Type])]
constructorList = do
    first <- constructor
    rest <- many $ do
        satisfy (\t -> t == PIPE || t == NEWLINE)  -- Allow | or newline
        constructor
    return (first : rest)

constructor :: Parser (String, [Type])
constructor = do
    name <- identifier
    types <- many typeAtom
    return (name, types)

typeAliasDeclaration :: Parser Declaration
typeAliasDeclaration = do
    token ALIAS
    name <- identifier
    typeVars <- many identifier
    token EQUALS
    typ <- typeAnnotation
    return $ TypeAlias name typeVars typ

functionAnnotation :: Parser Declaration
functionAnnotation = do
    name <- identifier
    token COLON
    typ <- typeAnnotation
    return $ TypeAnnotation name typ

functionDeclaration :: Parser Declaration
functionDeclaration = do
    name <- identifier
    patterns <- many pattern
    token EQUALS
    body <- expression
    return $ FunctionDecl name patterns body

-- Type Parser

typeAnnotation :: Parser Type
typeAnnotation = typeArrow

typeArrow :: Parser Type
typeArrow = do
    left <- typeApplication
    rest <- optional $ do
        arrowType <- (token ARROW >> return TArrow)
            <|> (token LINEAR_ARROW >> return TLinearArrow)
        right <- typeArrow
        return (arrowType, right)
    case rest of
        Nothing -> return left
        Just (constructor, right) -> return $ constructor left right

typeApplication :: Parser Type
typeApplication = do
    types <- some typeAtom
    
    case types of
        -- Single type, possibly with trailing !
        [t] -> do
            hasBang <- check BANG
            when hasBang $ void (token BANG)
            return $ if hasBang then TLinear t else t
        
        -- Linear type constructor followed by arguments: List! Int -> TLinear (List Int)
        (TLinear (TCon name []) : args) ->
            return $ TLinear (TCon name args)
        
        -- Regular type constructor with arguments: List Int
        (TCon name [] : args) -> do
            hasBang <- check BANG
            when hasBang $ void (token BANG)
            let appliedType = TCon name args
            return $ if hasBang then TLinear appliedType else appliedType
        
        -- Type variable used as constructor: a b c
        (TVar name : args) -> do
            hasBang <- check BANG
            when hasBang $ void (token BANG)
            let appliedType = TCon name args
            return $ if hasBang then TLinear appliedType else appliedType
        
        _ -> throwError $ ParseError "Invalid type application" (Position 0 0)

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
            
            -- Handle LINEAR_IDENT for types like List! Int
            LINEAR_IDENT name -> do
                _ <- token (LINEAR_IDENT name)
                let baseType = if isUpper (head name)
                              then TCon name []
                              else TVar name
                return $ TLinear baseType
            
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
binaryExpression = parseOperator <|> applicationExpression
  where
    parseOperator = do
        left <- applicationExpression
        rest <- optional $ do
            op <- operator
            right <- binaryExpression
            return (op, right)
        case rest of
            Nothing -> return left
            Just (op, right) -> return $ EBinOp op left right

operator :: Parser String
operator = do
    tok <- satisfy isOperator
    return $ case tokenType tok of
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
  where
    isOperator t = t `elem`
        [ PLUS, MINUS, MULTIPLY, DIVIDE, APPEND, CONS
        , PIPE, COMPOSE_LEFT, COMPOSE_RIGHT, COMPOSE
        , DOUBLE_EQUALS, NOT_EQUALS
        , LESS_THAN, GREATER_THAN, LESS_EQUAL, GREATER_EQUAL
        , AND, OR
        ]

applicationExpression :: Parser Expr
applicationExpression = do
    exprs <- some accessExpression
    case exprs of
        [e] -> return e
        (f:args) -> return $ foldl EApp f args
        [] -> throwError $ ParseError "Empty application" (Position 0 0)

accessExpression :: Parser Expr
accessExpression = do
    expr <- primaryExpression
    fields <- many $ do
        token DOT
        identifier
    return $ foldl EFieldAccess expr fields

primaryExpression :: Parser Expr
primaryExpression = literalExpression
    <|> variableExpression
    <|> listExpression
    <|> recordExpression
    <|> parensExpression

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
            LINEAR_IDENT name -> do
                token (LINEAR_IDENT name)
                return $ ELinearVar name
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
pattern = consPattern

consPattern :: Parser Pattern
consPattern = do
    left <- patternAtom
    rest <- optional $ do
        token CONS
        consPattern
    case rest of
        Nothing -> return left
        Just right -> return $ PCons left right

patternAtom :: Parser Pattern
patternAtom = wildcardPattern
    <|> literalPattern
    <|> variablePattern
    <|> listPattern
    <|> tuplePattern
    <|> constructorPattern
    <|> parensPattern

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
    tok <- peek
    case tok of
        Just t -> case tokenType t of
            IDENTIFIER name -> do
                token (IDENTIFIER name)
                return $ PVar name
            LINEAR_IDENT name -> do
                token (LINEAR_IDENT name)
                return $ PLinearVar name
            _ -> throwError $ ParseError "Expected variable pattern" (position t)
        Nothing -> throwError $ ParseError "Unexpected end" (Position 0 0)

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
    name <- identifier
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