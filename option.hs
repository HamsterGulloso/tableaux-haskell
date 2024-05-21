data Option a =
    None
    | Some a
    deriving(Show)

instance (Eq a) => Eq (Option a) where
    None == None = True
    Some x == Some y = x == y
    _ == _ = False

instance Functor Option where
    fmap _ None = None
    fmap f (Some s) = Some (f s)
