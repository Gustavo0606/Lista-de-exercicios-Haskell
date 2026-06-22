import System.IO
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

--(1) Redefina a função putStr :: String -> IO () 
--usando compreensão de listas e a função sequence_ :: [IO a] -> IO () 
--disponível no Prelude.
putString :: String -> IO ()
putString l = sequence_ [putChar x | x <- l]

--(2) Defina uma ação somador :: IO () que lê uma dada
--quantidade de inteiros do teclado, um por linha, e exiba sua soma.
somador :: IO ()
somador = do
    putStrLn "Quantos números? "
    qtdNum <- getLine
    total <- somarIO (read qtdNum :: Int)
    putStrLn ("O total é " ++ show total)
        where
            somarIO 0 = do
                return 0
            somarIO x = do
                atual <- getNum
                restante <- somarIO(x-1)

                return (atual + restante)
            getNum :: IO Int
            getNum = do
                numeroStr <- getLine
                let numeroInt = read numeroStr :: Int
                return numeroInt

--(3) Redefina a função somador usando a função sequence :: [IO a] -> IO [a]
--que executa uma lista de ações e retorna uma lista de resultados. 
somador2 :: IO ()
somador2 = do
    putStrLn "Quantos números? "
    qtdNum <- getLine
    total <- sequence(replicate (read qtdNum :: Int) getNum)
    putStrLn ("O total é " ++ show (sum total))
    where
        getNum :: IO Int
        getNum = do
            numeroStr <- getLine
            let numeroInt = read numeroStr :: Int
            return numeroInt

--(4) Tomando como base a função obterChar no material de aula, defina
--uma função chamada obterLinha :: IO String que se comporta exatamente como
--a função getLine :: IO String, mas permite a deleção de caracteres.
obterLinha :: IO String
obterLinha = auxiliar ""

auxiliar :: String -> IO String
auxiliar acumulado = do
    x <- obterChar
    case x of 
        '\n' -> do
            putChar '\n'
            return acumulado
        '\DEL' -> do
            if null acumulado then auxiliar ""
            else do 
                putStr "\b \b"
                auxiliar (init acumulado)
        c -> do
            putChar c
            auxiliar (acumulado ++ [c])


obterChar:: IO Char
obterChar = do hSetEcho stdin False
               x <- getChar
               hSetEcho stdin True
               return x
