import Text.Printf (printf)

main2 = do
   putStrLn "Seu nome e idade:"
   nome <- getLine
   idade <- getLine
   putStrLn $ printf "nome: %s, idade: %s" nome idade

main = 
    putStrLn "Seu nome:"
    >> getLine
    >>= \nome -> 
        getLine
        >>= \idade -> putStrLn $ printf "Falae %s, idade: %s" nome idade
