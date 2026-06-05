data Nat = Zero | Suc Nat deriving Show

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat a = Suc(int2nat(a-1))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Suc a) = 1 + nat2int a

somar :: Nat -> Nat -> Nat
somar Zero b = b
somar (Suc a) b = Suc(somar a b)

--(1) Semelhante à função somar, defina uma função de multiplicação recursiva para números naturais mult :: Nat -> Nat -> Nat. 
mult :: Nat -> Nat -> Nat
mult a Zero = Zero
mult a (Suc b) = somar a (mult a b)

--(2) O Prelude define o tipo Ordering que decide se o primeiro valor recebido como argumento é 
--menor (LT), igual (EQ) ou maior (GT) que o segundo argumento. 
--Usando essa função redefina a função existe :: Ord a => a -> Arvore a -> Bool para árvores binárias de busca.
data Arvore a = Folha a | No (Arvore a) a (Arvore a) deriving Show

existe :: Ord a => a -> Arvore a -> Bool
existe a (Folha y) = compare a y == EQ
existe a (No esq valor dir)
    |compare a valor == LT = existe a esq
    |compare a valor == GT = existe a dir
    |otherwise = True

--(3) Considere o seguinte tipo de árvores binárias: data ArvoreB a = Folha a | No (ArvoreB a) (ArvoreB a)
--Digamos que a árvore é balanceada se a quantidade de folhas do lado esquerdo e do lado direito de todos os nós são iguais ou sua diferença é no máximo 1
--Suas folhas são consideradas balanceadas por definição, Defina uma função balanceada :: Arvore a -> Bool que decide se uma árvore é balanceada ou não.
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

balanceada :: Tree a -> Bool
--caso base do balanceamento
balanceada (Leaf _) = True
--testar se a arvore e as subarvores estão balanceadas
balanceada (Node esq dir) = testar esq dir && balanceada esq && balanceada dir
    where 
        -- testar se a quantidade de folhas é a mesma na esquerda e direito, ou apenas uma folha de diferença
        testar a b = folhas a == folhas b || folhas a - folhas b == 1 || folhas b - folhas a == 1
        -- caso base das folhas
        folhas (Leaf _) = 1
        --recursão para contar a quantidade de folhas
        folhas (Node left right) = folhas left + folhas right

--(4) Defina a função balancear :: [a] -> Arvore a que converte uma lista não vazia
-- em uma árvore balanceada. Dica: primeiro defina uma função que divide uma lista em duas metades cujos tamanhos diferem em no máximo 1.
balancear :: [a] -> Tree a
balancear [x] = Leaf x
balancear l = Node (balancear esq) (balancear dir)
    where
        centro = div (length l) 2
        esq = take (centro) l
        dir = drop (centro) l

-- (5) Dada a definição data Expr = Val Int | Add Expr Expr defina a função
-- avaliar :: Expr -> Int tal que avaliar substitui cada construtor Val na expressão 
--pelo valor Int representado pelo construtor, e cada construtor Add pela aplicação da função (+).
data Expr = Val Int | Add Expr Expr

avaliar :: Expr -> Int
avaliar (Val a) = a
avaliar (Add a b) = avaliar a + avaliar b

--(6) Utilizando a definição data Expre = Value Int | Op Expre Expre defina a função de alta ordem
--folde :: (Int -> a) -> (a -> a -> a) -> Expre -> a
--tal que folde f g substitui cada construtor Val na expressão pela aplicação da função f ao valor representado pelo Val
-- e cada construtor Op pela aplicação da função g aos valores resultantes de ambas as expressões codificadas pelo construtor Op.
data Expre = Value Int | Op Expre Expre
folde :: (Int -> a) -> (a -> a -> a) -> Expre -> a
folde f _ (Value x) = f x
folde f g (Op z y) = g (folde f g z) (folde f g y)

--(7) Usando a função folde, defina a função eval :: Expre -> Int que avalia uma expressão para um valor inteiro
--Uma forma de enxergar o que a função eval deve fazer é refletir sobre a seguinte frase:
--como eu posso usar a função folde de forma que eval avalie a expressão assumindo que Op signifique a soma?
-- Por exemplo, se eu executasse eval (Op (Val 1) (Val 4)), assumindo que Op é a soma, ela deveria retornar 5.
-- Perceba que Op poderia representar qualquer operação sobre os valores, basta que você forneça a operação desejada para a função folde.
eval :: Expre -> Int
eval x = folde id (+) x