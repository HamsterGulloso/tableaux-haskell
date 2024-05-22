module Parse(parse) where

import ProofTree (Expression)

parse "" = Nothing
parse (' ':tail) = parse tail
parse ('(':tail) =
    let e1 = head ("(" ++ tail =~ "\\(.*\\)" ++ "") in
    Just e1
