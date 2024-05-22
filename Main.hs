module Main (
    main
) where

import Text.Printf (printf)
import System.IO (hFlush, stdout)

import Parse (parse)

console text = 
    putStr text
    >> hFlush stdout

help =
    "Ajudas!"

loop =
    let action "?" = putStrLn help >> loop
        action "!" = return ()
        action cmd = putStrLn (parse cmd) >> loop
    in do
    console "> "
    v <- getLine
    action v

main = do
    putStrLn "Bem vinde ao tableaux-Haskell!"
    putStrLn "Digite uma formula lógica abaixo e veja siua resolução."
    putStrLn "Ou digite '?' e veja como definimos as operções"
    loop
