module Tokenizer(
) where

data Token = 
    TLiteral Char
    | TAnd
    | TOr
    | TNot
    | TOpenParam
    | TCloseParam
    deriving(Show)

tokenize "" = []
tokenize ('(':tail) = TOpenParam : tokenize tail
tokenize (')':tail) = TCloseParam : tokenize tail
tokenize ('&':tail) = TAnd : tokenize tail
tokenize ('|':tail) = TOr : tokenize tail
tokenize ('!':tail) = TNot : tokenize tail
tokenize (' ':tail) = tokenize tail
tokenize (char:tail) =
    TLiteral char:tokenize tail

