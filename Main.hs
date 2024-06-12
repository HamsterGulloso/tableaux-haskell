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
    if isNothing parsed then
        "Formula ruim"
    else
        let Just exp = parsed 
            truePT = ProofTree.createTree [(exp, True)] 
            falsePT = ProofTree.createTree [(exp, False)] 
            evalTrue = ProofTree.evaluateTree truePT []
            evalFalse = ProofTree.evaluateTree falsePT []
        in
        printf "\n%s\n\n%s\n%s\n\n%s\n%s\n"
            ("Formula inserida: " ++ show exp)
            (case evalTrue of
                ProofTree.OK -> "Formula não é contraditória"
                _ -> "Formula é contraditória, " ++ show evalTrue)
            (show truePT)
            (case evalFalse of
                ProofTree.OK -> "Formula não é tauntológica"
                _ -> "Formula é tauntológica, " ++ show evalFalse)
            (show falsePT)

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
    putStrLn "Ou digite '?' e veja como definimos as operções"
    loop
