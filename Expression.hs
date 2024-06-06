module Expression (
    Expression(..)
) where

data Expression =
    Literal String
    | Not Expression
    | And Expression Expression
    | Or Expression Expression

instance Show Expression where
    show (Literal l) = l
    show (Not (Literal l)) = "¬" ++ l
    show (Not (Not e)) = "¬" ++ show (Not e)
    show (Not e) = "¬(" ++ show e ++ ")"
    show (And e1 e2) =
        let showinner (Literal l) = l
            showinner (Not e) = show (Not e)
            showinner (And e1 e2) = show (And e1 e2)
            showinner e = "(" ++ show e ++ ")"
        in
        showinner e1 ++ "∧" ++ showinner e2
    show (Or e1 e2) =
        let showinner (Literal l) = l
            showinner (Not e) = show (Not e)
            showinner (Or e1 e2) = show (Or e1 e2)
            showinner e = "(" ++ show e ++ ")"
        in
        showinner e1 ++ "∨" ++ showinner e2

instance Eq Expression where
    (==) (Literal a) (Literal b) = a == b
    (==) (Not a) (Not b) = a == b
    (==) (And a b) (And c d) =
        (a == c && b == d) ||
        (a == d && b == c)
    (==) (Or a b) (Or c d) =
        (a == c && b == d) ||
        (a == d && b == c)
    (==) _ _ = False
