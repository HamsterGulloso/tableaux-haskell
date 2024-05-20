data Fraction a =
    Fraction a a
    deriving(Show)

toFractional :: (Fractional a) => Fraction a -> a
toFractional (Fraction a b) =
    a / b
