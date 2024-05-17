data Vetor =
    Vetor Double Double
    deriving(Show, Eq)

data Ponto =
    Ponto Double Double
    deriving(Show, Eq)

data Forma =
    Circulo Ponto Double
    | Retangulo Ponto Ponto
    deriving (Show, Eq)

perimetro (Circulo _ r) = 2*pi*r
perimetro (Retangulo (Ponto x1 y1) (Ponto x2 y2)) =
    let width = abs (x2 - x1)
        height = abs (y2 - y1) in
    (width + height) * 2

area (Circulo _ r) = pi * (r ** 2)
area (Retangulo (Ponto x1 y1) (Ponto x2 y2)) =
    let width = abs (x2 - x1)
        height = abs (y2 - y1) in
    width * height
