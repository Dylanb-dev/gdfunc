{-# LANGUAGE OverloadedStrings #-}

-- | Megaparsec-based parser for GDFunc (phase 2b, in progress).
--
-- Produces the same AST as 'GDFunc.Parser' so downstream stages are
-- unaffected. Built on the legacy scanner's token stream via
-- 'GDFunc.Parser2.Stream'.
--
-- Layout handling: atom-level parsers accept an optional minimum
-- column. Constructs that open an indented block (case arms, let
-- bindings) install a column floor for their bodies. This lets a
-- body expression terminate naturally when the next non-newline
-- token is at the block's column or shallower — solving the classic
-- @x :: xs ->@ arm-boundary problem that the legacy parser hit.
module GDFunc.Parser2
    ( parseModule
    , parseFromTokens
    , Parser2Error
    ) where

import           Control.Monad             (void, when)
import           Data.Char                 (isLower, isUpper)
import           Data.Void                 (Void)
import qualified Data.Set                  as Set
import           Text.Megaparsec
    ( Parsec, (<|>), (<?>), between, choice, getInput, many, runParser
    , sepBy, sepBy1, eof, notFollowedBy, optional, some, try
    , token, errorBundlePretty, ParseErrorBundle
    )
import qualified Control.Monad.Combinators.Expr as E

import qualified GDFunc.Scanner            as Sc
import           GDFunc.Parser
    ( Module(..)
    , Declaration(..)
    , Type(..)
    , Expr(..)
    , Pattern(..)
    , LetBinding(..)
    , Import(..)
    )
import           GDFunc.Parser2.Stream     (TokenStream, mkTokenStream, unTokenStream)

type P = Parsec Void TokenStream

type Parser2Error = ParseErrorBundle TokenStream Void

-- ============================================================
-- Entry points
-- ============================================================

parseFromTokens :: FilePath -> [Sc.Token] -> Either Parser2Error Module
parseFromTokens path toks =
    let stripped = filter (not . isJustWhitespace) toks
    in runParser (sc *> moduleP <* eof) path (mkTokenStream stripped)

parseModule :: FilePath -> String -> Either String Module
parseModule path src = case Sc.scanTokens src of
    Left err   -> Left $ "scan: " ++ err
    Right toks -> case parseFromTokens path toks of
        Left bundle -> Left $ errorBundlePretty bundle
        Right m     -> Right m

-- ============================================================
-- Low-level token matchers
-- ============================================================

satisfyTok :: (Sc.Token -> Bool) -> P Sc.Token
satisfyTok p = token (\t -> if p t then Just t else Nothing) Set.empty

ttype :: Sc.TokenType -> P ()
ttype tt = void (satisfyTok ((== tt) . Sc.tokenType)) <?> show tt

identTok :: P String
identTok = do
    t <- satisfyTok $ \tok -> case Sc.tokenType tok of
        Sc.IDENTIFIER _ -> True
        _               -> False
    case Sc.tokenType t of
        Sc.IDENTIFIER s -> pure s
        _               -> fail "identifier"

upperIdent :: P String
upperIdent = try $ do
    s <- identTok
    case s of
        (c:_) | isUpper c -> pure s
        _                 -> fail "uppercase identifier"

lowerIdent :: P String
lowerIdent = try $ do
    s <- identTok
    case s of
        "_"                 -> fail "underscore"
        (c:_) | isLower c   -> pure s
        _                   -> fail "lowercase identifier"

intTok :: P Int
intTok = do
    t <- satisfyTok $ \tok -> case Sc.tokenType tok of
        Sc.NUMBER _ -> True
        _           -> False
    case Sc.tokenType t of
        Sc.NUMBER n -> pure n
        _           -> fail "number"

floatTok :: P Double
floatTok = do
    t <- satisfyTok $ \tok -> case Sc.tokenType tok of
        Sc.FLOAT _ -> True
        _          -> False
    case Sc.tokenType t of
        Sc.FLOAT f -> pure f
        _          -> fail "float"

stringTok :: P String
stringTok = do
    t <- satisfyTok $ \tok -> case Sc.tokenType tok of
        Sc.STRING _ -> True
        _           -> False
    case Sc.tokenType t of
        Sc.STRING s -> pure s
        _           -> fail "string"

charTok :: P Char
charTok = do
    t <- satisfyTok $ \tok -> case Sc.tokenType tok of
        Sc.CHAR _ -> True
        _         -> False
    case Sc.tokenType t of
        Sc.CHAR c -> pure c
        _         -> fail "char"

sc :: P ()
sc = void $ many (ttype Sc.NEWLINE)

lexeme :: P a -> P a
lexeme p = p <* sc

isJustWhitespace :: Sc.Token -> Bool
isJustWhitespace t = case Sc.tokenType t of
    Sc.COMMENT _ -> True
    Sc.INDENT _  -> True
    Sc.DEDENT _  -> True
    Sc.EOF       -> True
    _            -> False

-- ============================================================
-- Layout / column tracking
-- ============================================================

-- | Start column of the next token (or 'maxBound' if the stream is
-- empty). The scanner records the /end/ column of each token (one
-- past the last character), so we subtract the lexeme length to
-- recover the start column that layout cares about.
peekColumn :: P Int
peekColumn = do
    s <- getInput
    pure $ case unTokenStream s of
        (t:_) ->
            let endCol = Sc.column (Sc.position t)
                len    = length (Sc.lexeme t)
            in max 1 (endCol - len)
        []    -> maxBound

-- | Fail (consume nothing) if the next token's column is not strictly
-- greater than @minCol@.
requireIndentedBeyond :: Int -> P ()
requireIndentedBeyond minCol = when (minCol > 0) $ do
    c <- peekColumn
    when (c <= minCol) $ fail "expression body ended (dedent)"

-- ============================================================
-- Module
-- ============================================================

moduleP :: P Module
moduleP = do
    ttype Sc.MODULE
    name <- modulePathP
    expos <- optional exposingP
    sc
    imports <- many importP
    sc
    decls <- many (declP <* sc)
    pure $ Module name expos imports decls

modulePathP :: P [String]
modulePathP = lexeme (identTok `sepBy1` ttype Sc.DOT)

exposingP :: P [String]
exposingP = do
    ttype Sc.EXPOSING
    between (ttype Sc.LEFT_PAREN) (ttype Sc.RIGHT_PAREN) $
        lexeme identTok `sepBy` lexeme (ttype Sc.COMMA)

importP :: P Import
importP = do
    ttype Sc.IMPORT
    name <- modulePathP
    alias <- optional $ do
        ttype Sc.AS
        identTok
    expos <- optional exposingP
    sc
    pure $ Import name alias expos

-- ============================================================
-- Declarations
-- ============================================================

declP :: P Declaration
declP = choice
    [ try typeAliasDeclP
    , try typeDeclP
    , try typeAnnotationP
    , functionDeclP
    ]

typeDeclP :: P Declaration
typeDeclP = do
    ttype Sc.TYPE
    name <- upperIdent
    vars <- many (try lowerIdent)
    lexeme (ttype Sc.EQUALS)
    ctors <- constructorP `sepBy1` lexeme (ttype Sc.PIPE)
    pure $ TypeDecl name vars ctors

typeAliasDeclP :: P Declaration
typeAliasDeclP = do
    ttype Sc.TYPE
    ttype Sc.ALIAS
    name <- upperIdent
    vars <- many (try lowerIdent)
    lexeme (ttype Sc.EQUALS)
    ty   <- typeP
    pure $ TypeAlias name vars ty

constructorP :: P (String, [Type])
constructorP = do
    name <- upperIdent
    args <- many (try typeAtom)
    pure (name, args)

typeAnnotationP :: P Declaration
typeAnnotationP = try $ do
    name <- lowerIdent
    ttype Sc.COLON
    ty   <- typeP
    pure $ TypeAnnotation name ty

functionDeclP :: P Declaration
functionDeclP = do
    name <- lowerIdent
    pats <- many (try patternAtom)
    lexeme (ttype Sc.EQUALS)
    body <- exprP
    pure $ FunctionDecl name pats body

-- ============================================================
-- Patterns
-- ============================================================

-- | A full pattern, including @::@ (cons) at the top.
patternP :: P Pattern
patternP = do
    head' <- patternApp
    next <- optional $ try (lexeme (ttype Sc.CONS))
    case next of
        Nothing -> pure head'
        Just () -> PCons head' <$> patternP

-- | A constructor with arguments (or just an atom).
patternApp :: P Pattern
patternApp = choice
    [ try ctorWithArgsPattern
    , patternAtom
    ]

ctorWithArgsPattern :: P Pattern
ctorWithArgsPattern = do
    name <- upperIdent
    args <- some (try patternAtom)
    pure $ PCtor name args

patternAtom :: P Pattern
patternAtom = choice
    [ wildcardPattern
    , PVar <$> lowerIdent
    , PCtor <$> upperIdent <*> pure []
    , PInt <$> intTok
    , PString <$> stringTok
    , PChar <$> charTok
    , borrowPattern
    , listPattern
    , parensOrTuplePattern
    ]
  where
    borrowPattern = do
        ttype Sc.AMPERSAND
        PBorrow <$> patternAtom

wildcardPattern :: P Pattern
wildcardPattern = try $ do
    void $ satisfyTok $ \tok -> case Sc.tokenType tok of
        Sc.IDENTIFIER "_" -> True
        _                 -> False
    pure PWildcard

listPattern :: P Pattern
listPattern = do
    ttype Sc.LEFT_BRACKET
    items <- lexeme patternP `sepBy` lexeme (ttype Sc.COMMA)
    ttype Sc.RIGHT_BRACKET
    pure $ PList items

parensOrTuplePattern :: P Pattern
parensOrTuplePattern = between (ttype Sc.LEFT_PAREN) (ttype Sc.RIGHT_PAREN) $ do
    first <- lexeme patternP
    rest  <- many $ do
        lexeme (ttype Sc.COMMA)
        lexeme patternP
    pure $ case rest of
        [] -> PParens first
        _  -> PTuple (first : rest)

-- ============================================================
-- Types
-- ============================================================

typeP :: P Type
typeP = do
    left <- typeApp
    arr <- optional $ try $ lexeme (ttype Sc.ARROW)
    case arr of
        Nothing -> pure left
        Just () -> TArrow left <$> typeP

typeApp :: P Type
typeApp = choice
    [ borrowed
    , application
    ]
  where
    borrowed = do
        ttype Sc.AMPERSAND
        TBorrowed <$> typeApp
    application = do
        first <- typeAtom
        rest  <- many (try typeAtom)
        case (first, rest) of
            (t, [])                  -> pure t
            (TCon n [], args)        -> pure (TCon n args)
            (TVar n, args)           -> pure (TCon n args)
            _                        -> fail "invalid type application"

typeAtom :: P Type
typeAtom = choice
    [ try $ TVar <$> lowerIdent
    , TCon <$> upperIdent <*> pure []
    , parensOrTupleType
    ]

parensOrTupleType :: P Type
parensOrTupleType = between (ttype Sc.LEFT_PAREN) (ttype Sc.RIGHT_PAREN) $ do
    first <- lexeme typeP
    rest  <- many $ do
        lexeme (ttype Sc.COMMA)
        lexeme typeP
    pure $ case rest of
        [] -> first
        _  -> TTuple (first : rest)

-- ============================================================
-- Expressions
-- ============================================================

exprP :: P Expr
exprP = exprAt 0

-- | Parse an expression, but every atom inside must be at a column
-- strictly greater than @minCol@. Passing 0 disables the check.
exprAt :: Int -> P Expr
exprAt minCol = E.makeExprParser (appAt minCol) opTable

opTable :: [[E.Operator P Expr]]
opTable =
    [ [ binL Sc.MULTIPLY "*", binL Sc.DIVIDE "/"             ]
    , [ binL Sc.PLUS     "+", binL Sc.MINUS  "-"             ]
    , [ binL Sc.APPEND   "++", binR Sc.CONS  "::"            ]
    , [ binL Sc.LESS_EQUAL    "<=", binL Sc.GREATER_EQUAL ">="
      , binL Sc.LESS_THAN     "<",  binL Sc.GREATER_THAN  ">"
      , binL Sc.DOUBLE_EQUALS "==", binL Sc.NOT_EQUALS    "/="
      ]
    , [ binL Sc.AND "&&" ]
    , [ binL Sc.OR  "||" ]
    , [ binL Sc.PIPE "|>" ]
    ]
  where
    binL t op = E.InfixL $ EBinOp op <$ lexeme (ttype t)
    binR t op = E.InfixR $ EBinOp op <$ lexeme (ttype t)

appAt :: Int -> P Expr
appAt minCol = do
    head' <- atomAt minCol
    args  <- many (try (atomAt minCol))
    pure $ foldl EApp head' args

atomAt :: Int -> P Expr
atomAt minCol = do
    requireIndentedBeyond minCol
    atomExpr

atomExpr :: P Expr
atomExpr = choice
    [ ifExpr
    , letExpr
    , caseExpr
    , lambdaExpr
    , parensOrTupleExpr
    , listExpr
    , borrowExpr
    , try (EFloat <$> floatTok)
    , EInt <$> intTok
    , EString <$> stringTok
    , EChar <$> charTok
    , try varExpr
    , try ctorExpr
    ]

ifExpr :: P Expr
ifExpr = do
    ttype Sc.IF
    sc
    cond <- exprP
    sc
    ttype Sc.THEN
    sc
    thenE <- exprP
    sc
    ttype Sc.ELSE
    sc
    EIf cond thenE <$> exprP

letExpr :: P Expr
letExpr = do
    ttype Sc.LET
    sc
    bindings <- some (letBinding <* sc)
    ttype Sc.IN
    sc
    body <- exprP
    pure $ ELet bindings body

letBinding :: P LetBinding
letBinding = choice
    [ try letAnnotation
    , try letDestructure
    , letDef
    ]

letAnnotation :: P LetBinding
letAnnotation = do
    name <- lowerIdent
    ttype Sc.COLON
    ty   <- typeP
    pure $ LetAnnotation name ty

letDestructure :: P LetBinding
letDestructure = do
    pat <- destructurePattern
    lexeme (ttype Sc.EQUALS)
    rhs <- exprP
    pure $ LetDestructure pat rhs
  where
    -- Only patterns that won't be confused with simple-name bindings.
    destructurePattern = choice
        [ parensOrTuplePattern
        , listPattern
        ]

letDef :: P LetBinding
letDef = do
    name <- lowerIdent
    pats <- many (try patternAtom)
    lexeme (ttype Sc.EQUALS)
    rhs  <- exprP
    pure $ LetDef name pats rhs

caseExpr :: P Expr
caseExpr = do
    ttype Sc.CASE
    sc
    scrutinee <- exprP
    sc
    ttype Sc.OF
    sc
    armCol <- peekColumn
    arms <- some (try (caseArm armCol))
    pure $ ECase scrutinee arms

caseArm :: Int -> P (Pattern, Expr)
caseArm armCol = do
    -- Each arm's pattern must start at exactly the arm column.
    c <- peekColumn
    when (c /= armCol) $ fail "wrong arm column"
    pat <- patternP
    sc
    lexeme (ttype Sc.ARROW)
    body <- exprAt armCol
    sc
    pure (pat, body)

lambdaExpr :: P Expr
lambdaExpr = do
    ttype Sc.BACKSLASH
    pats <- some (try patternAtom)
    lexeme (ttype Sc.ARROW)
    ELambda pats <$> exprP

parensOrTupleExpr :: P Expr
parensOrTupleExpr = between (ttype Sc.LEFT_PAREN) (ttype Sc.RIGHT_PAREN) $ do
    first <- lexeme exprP
    rest  <- many $ do
        lexeme (ttype Sc.COMMA)
        lexeme exprP
    pure $ case rest of
        [] -> EParens first
        _  -> ETuple (first : rest)

listExpr :: P Expr
listExpr = do
    ttype Sc.LEFT_BRACKET
    items <- lexeme exprP `sepBy` lexeme (ttype Sc.COMMA)
    ttype Sc.RIGHT_BRACKET
    pure $ EList items

varExpr :: P Expr
varExpr = try $ do
    name <- lowerIdent
    -- Refuse to consume if this identifier is the start of a new
    -- declaration (a top-level @=@ / @:@) — that belongs to the
    -- next decl, not the current expression.
    notFollowedBy (ttype Sc.EQUALS <|> ttype Sc.COLON)
    pure (EVar name)

ctorExpr :: P Expr
ctorExpr = EVar <$> upperIdent

borrowExpr :: P Expr
borrowExpr = do
    ttype Sc.AMPERSAND
    EBorrow <$> atomExpr
