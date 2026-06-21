-- (1) Implemente uma função que receba uma lista de inteiros (que pode ou não estar ordenada)
-- e retorne uma lista ordenada em ordem crescente formada apenas pelos números ímpares da lista recebida.
impares :: [Int] -> [Int]
impares [] = []
impares x = ordenar(impar x)
    where
        ordenar [] = []
        ordenar [x] = [x]
        ordenar (y:ys) = ordenar(esquerda y ys) ++ [y] ++ ordenar(direita y ys)

        esquerda _ [] = []
        esquerda a (z:zs)
            |a > z = z: esquerda a zs
            |otherwise = esquerda a zs

        direita _ [] = []
        direita a (z:zs)
            |a <= z = z: direita a zs
            |otherwise = direita a zs
        
        impar [] = []
        impar (x:xs)
            |odd x = x: impar xs
            |otherwise = impar xs

-- (2) Defina uma função que retorne o elemento na n-ésima posição de uma lista.
posicao :: Int -> [a] -> a
posicao _ [] = error "lista vazia ou menor que o indice"
posicao 0 (x:xs) = x
posicao a (x:xs) = posicao (a-1) xs

-- (3) Defina uma função que repita as ocorrências até um determinado valor, no formato de uma lista
-- (Não é permitido usar replicate)
repetir :: Int -> [Int]
repetir 0 = []
repetir a = (replicar a a) ++ repetir(a-1)
    where
        replicar _ 0 = []
        replicar x y = x: replicar x (y-1)

-- (4) Construa uma função que cheque se o conteúdo de uma lista é um palíndromo:
palindromo ::Eq a => [a] -> Bool
palindromo a
    |a == reverse a = True
    |otherwise = False

-- (5) Construa uma função que retorne os n primeiros elementos da sequência de Fibonacci
fibonacci :: Int -> [Int]
fibonacci x = calcula_fibonacci 0 1 x
    where
        calcula_fibonacci _ _ 0 = []
        calcula_fibonacci a b c = a: calcula_fibonacci b (a+b) (c-1)

-- (6) Sem olhar as definições no Prelude, defina a seguintes funções de alta ordem:
-- a) Decide se todos os elementos de uma lista satisfazem um predicado:
todos :: (a -> Bool) -> [a] -> Bool
todos _ [] = True
todos a (x:xs)
    |a x = todos a xs
    |otherwise = False

-- b) Decide se algum elemento de uma lista satisfaz um predicado:
algum :: (a -> Bool) -> [a] -> Bool
algum _ [] = False
algum a (x:xs)
    |a x = True
    |otherwise = algum a xs

-- c) Selecione elementos de uma lista enquanto eles satisfazem um predicado:
pegueEnquanto :: (a -> Bool) -> [a] -> [a]
pegueEnquanto _ [] = []
pegueEnquanto a (x:xs)
    |a x = x: pegueEnquanto a xs
    |otherwise = []

-- c) Remove elementos de uma lista enquanto eles satisfazem um predicado:
removeEnquanto :: (a -> Bool) -> [a] -> [a]
removeEnquanto _ [] = []
removeEnquanto a (x:xs)
    |a x = removeEnquanto a xs
    |otherwise = (x:xs)

--(7) Redefina as funções map e filter usando foldr.
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr a = foldr funcao []
    where 
        funcao b c = a b: c
filterFoldr ::(a -> Bool) -> [a] -> [a]
filterFoldr a = foldr filtrar []
    where filtrar b c
            |a b = b: c
            |otherwise = c

--(8) Usando foldl, defina a função dec2int :: [Int] -> Int que converte uma lista de inteiros em um inteiro.
dec2int :: [Int] -> Int 
dec2int = foldl funcao 0
    where
        funcao a b = a*10 + b

--(9) Considere a função unfold que encapsula o padrão recursivo definido abaixo

unfold p h t x 
       | p x = []
       | otherwise = h x : unfold p h t (t x)
-- a função unfold produz uma lista vazia se o predicado é verdadeiro para o argumento passado em x
-- aso contrário, produz uma lista não vazia aplicando h a x, para formar a cabeça, e a função t aplicada a x que é processado recursivamente 
-- usando as mesmas regras, produzindo a cauda da lista.
--Redefina as funções map f e iterate f da biblioteca padrão usando a função unfold.

mapUnfold :: (a -> b) -> [a] -> [b]
mapUnfold a b = unfold (null) (\(x:xs) -> a x) (\(x:xs) -> xs) b

iterateUnfold :: (a -> a) -> a -> [a]
iterateUnfold a b = unfold (\_ -> False) id (a) b


--(10) Defina a função altMap :: (a -> b) -> (a -> b) -> [a] -> [b] que aplica de forma alternada
-- as duas funções que recebe como argumento a elementos sucessivos em uma lista.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap a b (x:xs) = a x : altMap b a xs

--(11) defina uma função de alta ordem chamada curry que converte uma função em um par (tupla) em uma versão currificada.
--Defina também uma função chamada uncurry que converte uma função currificada para dois argumentos em uma função que recebe um par (tupla).
curry :: ((a, b) -> c) -> a -> b -> c
curry (a, b) = a b