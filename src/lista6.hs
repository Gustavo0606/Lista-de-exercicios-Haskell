import System.Win32 (COORD(xPos))
-- (1) Defina um tipo de dado chamado DiaSemana que represente os sete dias da semana (de Segunda a Domingo). 
-- Em seguida, implemente a função ehFimDeSemana :: DiaSemana -> Bool
-- que retorna True se o dia fornecido for Sabado ou Domingo, e False para os demais dias da semana. 
data DiaSemana = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo deriving Show
ehFimDeSemana :: DiaSemana -> Bool
ehFimDeSemana Sabado = True
ehFimDeSemana Domingo = True
ehFimDeSemana _ = False

-- (2) Defina um tipo de dado Ponto2D que represente um ponto no plano 
-- cartesiano através de duas coordenadas do tipo Double.
-- Escreva uma função chamada distanciaOrigem :: Ponto2D -> Double 
-- que calcule a distância euclidiana desse ponto até a origem (0,0).
type Ponto2D = (Float, Float)
distanciaOrigem :: Ponto2D -> Float
distanciaOrigem a = sqrt((fst a * fst a)+(snd a * snd a))

--(3) Defina um tipo de dado chamado Cliente. 
--Um cliente pode ser uma PessoaFisica (que armazena o nome como String e a idade como Int)
--ou uma PessoaJuridica (que armazena a razão social como String e o ano de fundação como Int).
--Escreva uma função obterNome :: Cliente -> String que extraia e retorne apenas o nome/razão social do cliente, independentemente de seu tipo.
data Cliente = PessoaFisica String Int| PessoaJuridica String Int
obterNome :: Cliente -> String
obterNome (PessoaFisica a b) = a
obterNome (PessoaJuridica a b) = a

--(4) Crie sua própria estrutura de lista definindo um tipo ListaInt.
--Ela deve ter dois construtores: Vazia (representando o fim da lista)
--e No (que guarda um elemento do tipo Int e o restante da estrutura ListaInt)
--Em seguida, implemente de forma recursiva a função somaLista :: ListaInt -> Int 
--para somar todos os inteiros contidos nessa lista.
data ListaInt = Vazio | No Int ListaInt deriving Show
somaLista :: ListaInt -> Int
somaLista Vazio = 0
somaLista (No a b) = a + somaLista b

--(5) Crie o operador (>|) :: Int -> ListaInt -> ListaInt 
--que é capaz de inserir um elemento ao fim de uma ListaInt.
(>|) :: Int -> ListaInt -> ListaInt 
(>|) x Vazio = No x (Vazio)
(>|) x (No a b) = No a ((>|) x b)

--(6) Crie o operador (|<) :: Int -> ListaInt -> ListaInt que é capaz de
--inserir um elemento no início de uma ListaInt.
(|<) :: Int -> ListaInt -> ListaInt
(|<) x Vazio = No x (Vazio)
(|<) x (No a b) = No x (No a b)

--(7) Crie um tipo de dado CorSemaforo com três construtores sem argumentos: Verde, Amarelo e Vermelho.
-- Escreva uma função proximaCor :: CorSemaforo -> CorSemaforo que simule a transição automática de um semáforo de trânsito tradicional
--(onde Verde avança para Amarelo, Amarelo avança para Vermelho, e Vermelho retorna para Verde).
data CorSemaforo = Verde | Amarelo | Vermelho deriving Show
proximaCor :: CorSemaforo -> CorSemaforo
proximaCor Verde = Amarelo
proximaCor Amarelo = Vermelho
proximaCor Vermelho = Verde

--(8) Crie um tipo de dado parametrizado Opcional a que possua dois construtores: Nenhum e Dado a.
--Escreva uma função filtrarValores :: [Opcional a] -> [a] que receba uma lista nativa do Haskell
--composta por elementos do tipo Opcional a e retorne uma lista contendo apenas
-- os valores desempacotados que estavam dentro do construtor Dado, descartando todas as ocorrências de Nenhum.
data Opcional a = Nenhum | Dado a deriving Show
filtrarValores :: [Opcional a] -> [a]
filtrarValores [] = []
filtrarValores (Nenhum:xs) = filtrarValores xs
filtrarValores (Dado a:xs) = a: filtrarValores xs