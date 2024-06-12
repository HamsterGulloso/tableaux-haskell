module ProofTree (
    ProofTree(..),
    Eval(..),
    createTree,
    evaluate,
    evaluateTree
) where

import Expression

type Value = (Expression, Bool)

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


createTree :: [Value] -> ProofTree
createTree [] =
    EmptyNode
createTree ((Literal l, b):values) =
    Branch (Literal l, b) (createTree values)
createTree ((Not e, b):values) =
    Branch (Not e, b) (createTree (values ++ [(e, not b)]))
createTree ((And a b, True):values) =
    Branch (And a b, True) $ createTree (values ++ [(a, True), (b, True)])
createTree ((And a b, False):values) =
    Split {
        value = (And a b, False),
        leftBranch = createTree (values ++ [(a, False)]),
        rightBranch = createTree (values ++ [(b, False)])
    }
createTree ((Or a b, False):values) =
    Branch (Or a b, False) $ createTree (values ++ [(a, False), (b, False)])
createTree ((Or a b, True):values) =
    Split {
        value = (Or a b, True),
        leftBranch = createTree (values ++ [(a, True)]),
        rightBranch = createTree (values ++ [(b, True)])
    }

data Eval =
    Contradiction Expression
    | OK

instance Show Eval where
    show OK = "OK"
    show (Contradiction e) = "Contradição: " ++ show e

evaluate :: [Value] -> Eval
evaluate [] = OK
evaluate ((e, b):tail) =
    if elem (e, not b) tail then
        Contradiction e
    else
        evaluate tail

evaluateTree :: ProofTree -> [Value] -> Eval
evaluateTree EmptyNode v = evaluate v
evaluateTree (Branch (Literal l, b) n) values =
    evaluateTree n ((Literal l, b):values)
evaluateTree (Branch _ n) v = evaluateTree n v
evaluateTree (Split (Literal lit, b) left right) values =
    let evalLeft = evaluateTree left ((Literal lit, b):values)
        evalRight = evaluateTree right ((Literal lit, b):values)
    in
    case evalLeft of
        Contradiction _ -> evalRight
        OK -> OK
evaluateTree (Split _ left right) values =
    let evalLeft = evaluateTree left values
        evalRight = evaluateTree right values
    in
    case evalLeft of
        Contradiction _ -> evalRight
        OK -> OK


printTree :: ProofTree -> String
printTree tree = printTreeIntern tree 0


printTreeIntern :: ProofTree -> Int -> String
printTreeIntern EmptyNode _ = ""
printTreeIntern (Branch (expression, bool) EmptyNode) indent =
    (if bool then "⊤" else "⊥") ++ ":" ++ show expression
printTreeIntern (Branch (expression, bool) next) indent =
    (if bool then "⊤" else "⊥") ++ ":" ++ show expression ++ " → " ++ printTreeIntern next indent
printTreeIntern (Split (expression, bool) left right) indent =
    (if bool then "⊤" else "⊥") ++ ":" ++ show expression ++ "\n" ++
        replicate (indent + 4) ' ' ++ printTreeIntern left (indent + 4) ++ "\n" ++
        replicate (indent + 4) ' ' ++ printTreeIntern right (indent + 4)
