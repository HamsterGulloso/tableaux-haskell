module ProofTree (
) where

import Expression
import Data.Maybe (isJust)

data ProofTree = 
    Branch {
        expression :: Expression,
        value :: Bool,
        open :: Bool,
        leftBranch :: ProofTree,
        rightBranch :: ProofTree
    }
    | EmptyNode

type Value = (Expression, Bool)

searchOpen :: ProofTree -> Maybe ProofTree
searchOpen EmptyNode = Nothing
searchOpen b =
    let lres = searchOpen (leftBranch b)
        rres = searchOpen (rightBranch b) in
    if open b then
        Just b
    else if isJust lres then
        lres
    else if isJust rres then
        rres
    else
        Nothing

    

createTree :: ProofTree -> Expression -> ProofTree
createTree p  = Leaf 
