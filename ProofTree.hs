module ProofTree (
    ProofTree(..),
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
    show EmptyNode = ""
    show (Branch v EmptyNode) = show v
    show (Branch v n) = show v ++ "->" ++ show n
    show (Split v l r) = show v ++ "->" ++ "{" ++ show l ++ "}{" ++ show r ++ "}"


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
    show (Contradiction e) = "Contradiction: " ++ show e

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
        Contradiction _ -> evalLeft
        OK -> evalRight
evaluateTree (Split _ left right) values =
    let evalLeft = evaluateTree left values
        evalRight = evaluateTree right values
    in
    case evalLeft of
        Contradiction _ -> evalLeft
        OK -> evalRight
