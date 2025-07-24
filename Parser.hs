module Parser(parse) where

import Text.Printf

import Expression (Expression(..))
import Tokenizer 
import Data.Text.Internal.Fusion (stream)

parse :: String -> Either String Expression
parse stream =
    tokenize stream >>= \tokens ->
        let res = parseTokens [] tokens
            error_msg fe nut = printf
                "Expressão ruim\nExpressão formada: %s\nTokens não utilizados: %s"
                (show fe)
                (show nut) 
        in
        case length res of
            0 -> Left "Expressão vazia"
            1 -> Right (head res)
            _ ->
                let formed_expression = head res
                    non_used_tokens = tail res
                in
                Left $ error_msg formed_expression non_used_tokens

parseTokens :: [Expression] -> [Token] -> [Expression]
parseTokens stack [] = stack
parseTokens stack (TLiteral str:tokens) =
    parseTokens (Literal str:stack) tokens
parseTokens (top:stack) (TNot:tokens) =
    parseTokens (Not top:stack) tokens
parseTokens (t2:t1:stack) (TAnd:tokens) =
    parseTokens (And t1 t2:stack) tokens
parseTokens (t2:t1:stack) (TOr:tokens) =
    parseTokens (Or t1 t2:stack) tokens
parseTokens (t2:t1:stack) (TArrow:tokens) =
    parseTokens (Then t1 t2:stack) tokens
parseTokens _ _ = []
