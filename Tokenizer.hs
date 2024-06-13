module Tokenizer(
    tokenize,
    Token(..)
) where

data Token = 
    TLiteral [Char]
    | TAnd
    | TOr
    | TNot
    | TArrow
    deriving(Show)

isAlphaNumeric = (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))

tokenize "" = Just []
tokenize ('&':tail) =
    tokenize tail >>= \t -> 
        Just $ TAnd:t
tokenize ('|':tail) =
    tokenize tail >>= \t -> 
        Just $ TOr:t
tokenize ('!':tail) =
    tokenize tail >>= \t -> 
        Just $ TNot:t
tokenize (' ':tail) = tokenize tail
tokenize ('-':'>':tail) =
    tokenize tail >>= \tokens ->
        Just $ TArrow : tokens
tokenize (c1:c2:tail) =
    tokenize (c2:tail) >>= \t ->
        if isAlphaNumeric c1 then
            if isAlphaNumeric c2 then
                let ((TLiteral str):tokens) = t in
                Just $ TLiteral (c1:str): tokens
            else
                Just $ TLiteral [c1]:t
        else Nothing
tokenize (c1:tail) =
    if isAlphaNumeric c1 then
        Just [TLiteral [c1]]
    else
        Nothing
