module ProofTree (
    ProofTree(..),
    Eval(..),
    createTree,
    evaluate,
    evaluateTree
) where

import Expression
import Distribution.FieldGrammar (FieldGrammar(booleanFieldDef))
import Text.Read.Lex (numberToFixed)

type Value = (Expression, Bool, [Int])

printNumber number =
    foldl (\acc n -> acc ++ "." ++ show n) (show (head number)) (tail number)

printValue (expression, bool, number) =
    "[" ++
    printNumber number ++
    " " ++
    (if bool then "⊤" else "⊥") ++
    ":" ++
    show expression ++
    "]"

data ProofTree = 
    Split {
        value :: Value,
        leftBranch :: ProofTree,
        rightBranch :: ProofTree
    } |
    Branch {
        value :: Value,
        next :: ProofTree
    } |
    EmptyNode

instance Show ProofTree where
    show = printTree

createTree :: Expression -> Bool -> ProofTree
createTree e b = createTreeIntern [(e, b, [1])]

createTreeIntern :: [Value] -> ProofTree
createTreeIntern [] =
    EmptyNode
createTreeIntern ((Literal l, b, n):values) =
    Branch (Literal l, b, n) (createTreeIntern values)
createTreeIntern ((Not e, b, n):values) =
    let next = case values of 
            [] -> init n ++ [last n + 1]
            _ -> let (_, _, number) = last values in init number ++ [last number + 1]
    in
    Branch (Not e, b, n) (createTreeIntern (values ++ [(e, not b, next)]))
createTreeIntern ((And a b, True, n):values) =
    let next = case values of 
            [] -> init n ++ [last n + 1]
            _ -> let (_, _, number) = last values in init number ++ [last number + 1]
        nnext = init next ++ [last next + 1]
    in
    Branch (And a b, True, n) $ createTreeIntern (values ++ [(a, True, next), (b, True, nnext)])
createTreeIntern ((And a b, False, n):values) =
    let next = case values of 
            [] -> init n ++ [last n + 1]
            _ -> let (_, _, number) = last values in init number ++ [last number + 1]
        nnext = init next ++ [last next + 1]
    in
    Split {
        value = (And a b, False, n),
        leftBranch = createTreeIntern (values ++ [(a, False, next ++ [1])]),
        rightBranch = createTreeIntern (values ++ [(b, False, nnext ++ [1])])
    }
createTreeIntern ((Or a b, False, n):values) =
    let next = case values of 
            [] -> init n ++ [last n + 1]
            _ -> let (_, _, number) = last values in init number ++ [last number + 1]
        nnext = init next ++ [last next + 1]
    in
    Branch (Or a b, False, n) $ createTreeIntern (values ++ [(a, False, next), (b, False, nnext)])
createTreeIntern ((Or a b, True, n):values) =
    let next = case values of 
            [] -> init n ++ [last n + 1]
            _ -> let (_, _, number) = last values in init number ++ [last number + 1]
        nnext = init next ++ [last next + 1]
    in
    Split {
        value = (Or a b, True, n),
        leftBranch = createTreeIntern (values ++ [(a, True, next ++ [1])]),
        rightBranch = createTreeIntern (values ++ [(b, True, nnext ++ [1])])
    }
createTreeIntern ((Then a b, False, n):values) =
    let next = case values of 
            [] -> init n ++ [last n + 1]
            _ -> let (_, _, number) = last values in init number ++ [last number + 1]
        nnext = init next ++ [last next + 1]
    in
    Branch (Then a b, False, n) $ createTreeIntern (values ++ [(a, True, next), (b, False, nnext)])
createTreeIntern ((Then a b, True, n):values) =
    let next = case values of 
            [] -> init n ++ [last n + 1]
            _ -> let (_, _, number) = last values in init number ++ [last number + 1]
        nnext = init next ++ [last next + 1]
    in
    Split {
        value = (Then a b, True, n),
        leftBranch = createTreeIntern (values ++ [(a, False, next ++ [1])]),
        rightBranch = createTreeIntern (values ++ [(b, True, nnext ++ [1])])
    }

data Eval =
    Contradiction [(Value, Value)]
    | OK

instance Show Eval where
    show OK = "OK"
    show (Contradiction c) = 
        let showCList [] = ""
            showCList [((_, _, n1),(_, _, n2))] = printNumber n1 ++ " e " ++ printNumber n2
            showCList (((_, _, n1),(_, _, n2)):tail) = printNumber n1 ++ " e " ++ printNumber n2  ++ ", " ++ showCList tail
        in
        "Contradições: " ++ showCList c

evaluate :: [Value] -> Eval
evaluate [] = OK
evaluate (v:tail) =
    let search = foldl
            (\(e1, b1, n1) (e2, b2, n2) ->
                if 
                    v == (e1, b1, n1) &&
                    (e1 == e2 && b1 == not b2)
                then
                    (e2, b2, n2)
                else
                    (e1, b1, n1)
            )
            v
            tail
        found = search /= v
    in
    if found then
        case evaluate tail of
            Contradiction c -> Contradiction $ (v, search):c
            OK -> Contradiction [(v, search)]
    else
        evaluate tail

evaluateTree :: ProofTree -> Eval
evaluateTree p = evaluateTreeIntern p []

evaluateTreeIntern :: ProofTree -> [Value] -> Eval
evaluateTreeIntern EmptyNode v = evaluate v
evaluateTreeIntern (Branch (Literal l, b, number) n) values =
    evaluateTreeIntern n ((Literal l, b, number):values)
evaluateTreeIntern (Branch _ n) v = evaluateTreeIntern n v
evaluateTreeIntern (Split (Literal lit, b, number) left right) values =
    let evalLeft = evaluateTreeIntern left ((Literal lit, b, number):values)
        evalRight = evaluateTreeIntern right ((Literal lit, b, number):values)
    in
    case evalLeft of
        OK -> evalRight
        Contradiction cleft -> case evalRight of
            Contradiction cright -> Contradiction (cright ++ cleft)
            OK -> evalLeft
evaluateTreeIntern (Split _ left right) values =
    let evalLeft = evaluateTreeIntern left values
        evalRight = evaluateTreeIntern right values
    in
    case evalLeft of
        OK -> evalRight
        Contradiction cleft -> case evalRight of
            Contradiction cright -> Contradiction (cright ++ cleft)
            OK -> evalLeft


printTree :: ProofTree -> String
printTree tree = printTreeIntern tree 0


printTreeIntern :: ProofTree -> Int -> String
printTreeIntern EmptyNode _ = ""
printTreeIntern (Branch value EmptyNode) indent =
    printValue value
printTreeIntern (Branch value next) indent =
    printValue value ++ " → " ++ printTreeIntern next indent
printTreeIntern (Split value left right) indent =
    printValue value ++ "\n" ++
        replicate (indent + 4) ' ' ++ printTreeIntern left (indent + 4) ++ "\n" ++
        replicate (indent + 4) ' ' ++ printTreeIntern right (indent + 4)
