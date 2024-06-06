module Parser(parse) where

import Expression (Expression(..))
import Tokenizer 

parse :: String -> Maybe Expression
parse str =
    tokenize str >>= \tokens ->
        let res = parseTokens [] tokens in
        if length res /= 1 then
            Nothing
        else
            Just (head res)

parseTokens :: [Expression] -> [Token] -> [Expression]
parseTokens stack [] = stack
parseTokens stack (TLiteral str:tokens) =
    parseTokens (Literal str:stack) tokens
parseTokens (top:stack) (TNot:tokens) =
    parseTokens (Not top:stack) tokens
parseTokens (t1:t2:stack) (TAnd:tokens) =
    parseTokens (And t1 t2:stack) tokens
parseTokens (t1:t2:stack) (TOr:tokens) =
    parseTokens (Or t1 t2:stack) tokens
parseTokens (t1:t2:stack) (TArrow:tokens) =
    parseTokens (Or t2 (Not t1):stack) tokens
parseTokens _ _ = []
