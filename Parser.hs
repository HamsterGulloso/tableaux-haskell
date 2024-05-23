module Parser(parse) where

import ProofTree (Expression(..))
import Tokenizer 

parse :: String -> Maybe Expression
parse str =
    let res = parseTokens [] (tokenize str) in
    if length res /= 1 then
        Nothing
    else
        Just (head res)

parseTokens :: [Expression] -> [Token] -> [Expression]
parseTokens stack [] = stack
parseTokens stack (TLiteral char:tokens) =
    parseTokens (Literal [char]:stack) tokens
parseTokens (top:stack) (TNot:tokens) =
    parseTokens (Not top:stack) tokens
parseTokens (t1:t2:stack) (TAnd:tokens) =
    parseTokens (And t1 t2:stack) tokens
parseTokens (t1:t2:stack) (TOr:tokens) =
    parseTokens (Or t1 t2:stack) tokens
parseTokens _ _ = []
