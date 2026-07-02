--(1) Crie um tipo de dados Estacao para representar as quatro estações do ano. Em seguida, crie um tipo Clima com dois construtores: 
--Chuvoso (que não recebe parâmetros) e Temperado (que recebe um valor de temperatura em Float). 
--Implemente a função ajustarClima :: Estacao -> Clima que retorna o clima padrão para uma estação
data Clima = Chuvoso | Temperado Float deriving Show
data Estacao = Verao | Outono | Inverno | Primavera
ajustarClima :: Estacao -> Clima
ajustarClima Inverno = Chuvoso
ajustarClima Outono = Chuvoso
ajustarClima Verao = Temperado 35.0
ajustarClima Primavera = Temperado 30.0

--(2)Para evitar bugs de conversão de moedas, crie dois tipos separados usando newtype: Real e Dolar, ambos encapsulando um valor Double.
--Escreva uma função converterRealParaDolar :: Double -> Real -> Dolar, onde o primeiro parâmetro é a taxa de câmbio (quanto vale 1 dólar em reais)
-- e o segundo é o valor em Real. O retorno deve ser do tipo Dolar.
newtype Dolar = Dolar Double deriving Show
newtype Reais = Reais Double
converterRealParaDolar :: Double -> Reais -> Dolar
converterRealParaDolar a (Reais b) = Dolar((b/a))

--(3)Implemente a função altura :: Arvore a -> Int que calcula a altura da árvore. Uma Folha tem altura 1
--e um No tem a altura calculada pelo maior caminho entre suas subárvores esquerda e direita, somado a 1.
data Arvore a = Folha a | No (Arvore a) a (Arvore a)
altura :: Arvore a -> Int
altura (Folha _) = 1
altura (No esquerda _ direita)
    |esq > dir = 1 + esq
    |otherwise = 1 + dir
    where
        esq = altura esquerda
        dir = altura direita

--(4)Utilizando a mesma estrutura de Arvore do exercício anterior, escreva uma função contarFolhas :: Arvore a -> Int
--que devolve a quantidade exata de nós do tipo Folha existentes na estrutura da árvore mapeada.
contarFolhas :: Arvore a -> Int
contarFolhas (Folha _) = 1
contarFolhas (No esquerda _ direita) = contarFolhas esquerda + contarFolhas direita

--(5)Crie um tipo de dados para representar as cartas de um baralho simplificado:
--Sem usar o deriving, escreva manualmente a declaração da instância de Eq para o tipo Naipe, para que o Haskell saiba comparar se dois naipes são iguais.
data Naipe = Copas | Espadas | Ouros | Paus
instance Eq Naipe where
    Copas == Copas = True
    Espadas == Espadas = True
    Ouros == Ouros = True
    Paus == Paus = True
    _ == _ = False

--(6)Considere o tipo de dados que representa a prioridade de uma tarefa:
--Implemente manualmente a instância da classe Ord para este tipo (instance Ord Prioridade where).
--Defina o comportamento de forma que Baixa < Media e Media < Alta. Você precisará implementar também a instância de Eq para ele primeiro (ou usar deriving Eq).
data Prioridade = Baixa | Media | Alta deriving Eq
instance Ord Prioridade where
    Baixa < Media = True
    Baixa < Alta = True
    Media < Alta = True
    _ < _ = False
    x <= y = (x < y) || (x == y)

    Media > Baixa = True
    Alta > Baixa = True
    Alta > Media = True
    _ > _ = False
    x >= y = (x > y) || (x == y)
