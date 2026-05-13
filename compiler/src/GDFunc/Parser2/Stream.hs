{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | A 'Stream' instance over the legacy 'GDFunc.Scanner.Token' list,
-- so megaparsec can drive 'GDFunc.Parser2'. Each token already
-- carries its own 'Position'; we forward that into megaparsec's
-- 'SourcePos' so error messages point at the right place.
module GDFunc.Parser2.Stream
    ( TokenStream(..)
    , mkTokenStream
    ) where

import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy(..))
import Text.Megaparsec
import qualified GDFunc.Scanner as Sc

newtype TokenStream = TokenStream { unTokenStream :: [Sc.Token] }
    deriving stock (Eq, Show)

mkTokenStream :: [Sc.Token] -> TokenStream
mkTokenStream = TokenStream

instance Stream TokenStream where
    type Token  TokenStream = Sc.Token
    type Tokens TokenStream = [Sc.Token]

    tokenToChunk  Proxy x  = [x]
    tokensToChunk Proxy xs = xs
    chunkToTokens Proxy    = id
    chunkLength   Proxy    = length
    chunkEmpty    Proxy    = null

    take1_ (TokenStream [])     = Nothing
    take1_ (TokenStream (t:ts)) = Just (t, TokenStream ts)

    takeN_ n s@(TokenStream ts)
        | n <= 0    = Just ([], s)
        | null ts   = Nothing
        | otherwise = let (a, b) = splitAt n ts in Just (a, TokenStream b)

    takeWhile_ p (TokenStream ts) =
        let (a, b) = span p ts in (a, TokenStream b)

instance VisualStream TokenStream where
    showTokens Proxy =
        unwords . map (showTok . Sc.tokenType) . NE.toList
      where
        showTok (Sc.IDENTIFIER s) = s
        showTok (Sc.NUMBER n)     = show n
        showTok (Sc.FLOAT f)      = show f
        showTok (Sc.STRING s)     = show s
        showTok (Sc.CHAR c)       = show c
        showTok t                 = show t
    tokensLength Proxy = NE.length

instance TraversableStream TokenStream where
    reachOffset o pst =
        let
            input       = unTokenStream (pstateInput pst)
            offsetDelta = max 0 (o - pstateOffset pst)
            remaining   = drop offsetDelta input
            srcName     = sourceName (pstateSourcePos pst)
            newPos      = case remaining of
                (t:_) ->
                    SourcePos srcName
                        (mkPos $ max 1 (Sc.line (Sc.position t)))
                        (mkPos $ max 1 (Sc.column (Sc.position t)))
                [] -> pstateSourcePos pst
        in
            ( Just "<tokens>"
            , pst
                { pstateInput     = TokenStream remaining
                , pstateOffset    = max o (pstateOffset pst)
                , pstateSourcePos = newPos
                }
            )
