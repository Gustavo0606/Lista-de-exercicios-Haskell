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
