data Arvore a =
    ArvoreVazia
    | Arvore {
        valor :: a,
        esquerda :: Arvore a,
        direita :: Arvore a
    }
instance Show a => Show (Arvore a)  where
    show ArvoreVazia = "{}"
    show (Arvore v e d) = "{" ++ show v ++ ":" ++ show e ++ show d ++ "}"
instance Functor Arvore where
    fmap _ ArvoreVazia = ArvoreVazia
    fmap f (Arvore v e d) = Arvore (f v) (fmap f e) (fmap f d)
    

insere ArvoreVazia item =
    Arvore{
        valor = item,
        esquerda = ArvoreVazia,
        direita = ArvoreVazia
    }
insere (Arvore v e d) item =
    if item < v then
        Arvore v (insere e item) d
    else
        Arvore v e (insere d item)

pertence ArvoreVazia x = read "False" :: Bool
pertence (Arvore v e d) x
    | x == v = True
    | x < v = pertence e x
    | otherwise = pertence d x
