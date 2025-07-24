module Main (
    main
) where

import Text.Printf (printf)
import System.IO (hFlush, stdout)
import Parser (parse)
import qualified ProofTree
import Data.Maybe (isNothing)

evaluate cmd =
    let parsed = parse cmd in
    case parsed of
        Left error -> printf "\n%s\n" error
        Right exp -> 
            let truePT = ProofTree.createTree exp True 
                falsePT = ProofTree.createTree exp False 
                evalTrue = ProofTree.evaluateTree truePT
                evalFalse = ProofTree.evaluateTree falsePT
            in
            printf "\n%s\n\n%s\n%s\n\n%s\n%s\n\n%s"
                ("Formula inserida: " ++ show exp)
                (show truePT)
                (show evalTrue)
                (show falsePT)
                (show evalFalse)
                (case (evalTrue, evalFalse) of
                    (ProofTree.OK, ProofTree.Contradiction _) -> "A formula é tauntológica"
                    (ProofTree.Contradiction _, ProofTree.OK) -> "A formula é contraditória"
                    _ -> "A formula é satisfazivel" )

console text = 
    putStr text
    >> hFlush stdout

help =
    "Nosso interpretador segue a notação polonesa reversa\n" ++
    "'&' => '∧'\n" ++
    "'|' => '∨'\n" ++
    "'!' => '¬'\n" ++
    "'->' => '→'\n" ++
    "Ex.: 'a b & c | a ! ->' => '(a∧b)∨c→¬a'\n" ++
    "Caso queira sair digite apenas '!'"

loop =
    let action "?" = putStrLn help >> loop
        action "!" = return ()
        action cmd = 
            putStrLn (evaluate cmd) >> loop
    in do
    console "> "
    v <- getLine
    action v

main = do
    putStrLn "Bem vinde ao tableaux-Haskell!"
    putStrLn "Digite uma formula lógica abaixo e veja sua resolução."
    putStrLn "Ou digite '?' e veja como definimos as operações"
    loop
