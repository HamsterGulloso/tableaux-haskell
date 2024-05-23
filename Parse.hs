module Parse(parse) where

import ProofTree (Expression(..))

-- parse "" = Nothing
-- parse (' ':tail) = parse tail
-- parse ('!':tail) =
--     parse tail
--     >>= \t -> Just $ Not t
-- parse ('(':tail) =
--     let inner (exp, counter) char =
--             if counter > 0 then
--                 case char of
--                     '(' -> (exp ++ [char], counter + 1)
--                     ')' -> if counter == 1 then 
--                             (exp, counter - 1)
--                         else
--                             (exp ++ [char], counter - 1)
--                     _ -> (exp ++ [char], counter)
--             else (exp, counter)
--         (innerExp, conter) = foldl inner ("", 1) tail in
--     parse innerExp
-- parse (char:tail) 
--     | elem char ['a'..'z'] =
--         case parse tail of
--             Nothing -> Just $ Literal [char]
--             Just (Literal l) -> Just (Literal $ char:l)
--             _ -> Nothing
--     | elem char ['^', 'V'] =
--         Nothing

-- reverse polish parser
parse = stackParse []

stackParse :: [Expression] -> String -> [Expression]
stackParse s "" = s
stackParse s (' ':tail) = stackParse s tail
stackParse (top:stack) ('!':tail) =
    stackParse ((Not top) : stack) tail
stackParse (t1:t2:stack) ('&':tail) = 
    stackParse ((And t1 t2):stack) tail
stackParse (t1:t2:stack) ('^':tail) =
    stackParse ((Or t1 t2):stack) tail
stackParse stack (char:tail) = 
    stackParse ((Literal [char]):stack) tail
