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

isAlphaNumeric = (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_@#$%*"))

tokenize :: String -> Either String [Token] 
tokenize "" = Right []
tokenize ('&':tail) =
    tokenize tail >>= \t -> 
        Right $ TAnd:t
tokenize ('|':tail) =
    tokenize tail >>= \t -> 
        Right $ TOr:t
tokenize ('!':tail) =
    tokenize tail >>= \t -> 
        Right $ TNot:t
tokenize (' ':tail) = tokenize tail
tokenize ('-':'>':tail) =
    tokenize tail >>= \tokens ->
        Right $ TArrow : tokens
tokenize (c1:c2:tail) =
    tokenize (c2:tail) >>= \t ->
        if isAlphaNumeric c1 then
            if isAlphaNumeric c2 then
                let ((TLiteral str):tokens) = t in
                Right $ TLiteral (c1:str): tokens
            else
                Right $ TLiteral [c1]:t
        else if isAlphaNumeric c2 then
            let ((TLiteral str):tokens) = t in
            Left $ "Token inválido: " ++ (c1:str)
        else
            Left $ "Token inválido: " ++ [c1]
            
tokenize (c1:tail) =
    if isAlphaNumeric c1 then
        Right [TLiteral [c1]]
    else
        Left $ "Token inválido: " ++ [c1]
