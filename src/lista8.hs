import Data.Char (toUpper)

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

--(7)Escreva uma ação chamada ecoChar :: IO () que lê um único caractere do teclado utilizando getChar e
-- em seguida, imprime esse mesmo caractere na tela duas vezes seguidas utilizando putChar.
ecoChar :: IO ()
ecoChar = do
    caracter <- getChar
    putChar(caracter)
    putChar(caracter)

--(8)Implemente sua própria versão da função que imprime uma string na tela, sem usar a putStr nativa. Chame-a de 
--imprimirString :: String -> IO (). Você deve implementá-la de forma recursiva utilizando apenas casamento de padrões e a primitiva putChar.
imprimirString :: String -> IO ()
imprimirString [] = return()
imprimirString (x:xs) = do
    putChar(x)
    imprimirString xs

--(9)Escreva uma ação chamada . Ela deve pedir para o usuário digitar uma linha de texto (utilizando getLine).
--O programa deve calcular o comprimento da string digitada e exibir na tela a mensagem: "Você digitou X caracteres.", onde X é o número de caracteres.
contarEntrada :: IO ()
contarEntrada = do
    putStrLn ("Digite uma linha de texto")
    texto <- getLine
    putStrLn("Você digitou " ++ show(somar texto) ++" caracteres")
    where
        somar [] = 0
        somar (x:xs) = 1 + somar xs

--(10) Escreva um pequeno programa interativo chamado menu :: IO (). Ele deve exibir três opções na tela:
--1. Saudação (Imprime "Olá, Mundo!")
--2. Despedida (Imprime "Até logo!")
--3. Sair (Encerra o programa)
--O programa deve ler a opção desejada. Se o usuário escolher 1 ou 2, o programa executa a ação correspondente e chama a
-- função menu novamente de forma recursiva (um loop de menu). Se escolher 3, o programa termina.
menu :: IO ()
menu = do
    putStrLn("1. Saudação")
    putStrLn("2. Despedida")
    putStrLn("3. Sair")
    entrada <- getLine
    loop(entrada)
    where
        loop "3" = return()
        loop "1" = do
            putStrLn("Olá, Mundo!")
            menu
        loop "2" = do
            putStrLn("Até logo!")
            menu
        loop _ = do
            menu
        
--(11)Escreva uma ação chamada repetirAteSair :: IO () que funciona como um eco contínuo. 
--Ela lê uma linha de texto do usuário e a imprime de volta em caixa alta (letras maiúsculas). 
--O loop só deve ser interrompido se o usuário digitar a palavra "SAIR".
repetirAteSair :: IO ()
repetirAteSair = do
    texto <- getLine
    loop(alto(texto))
    where
        loop "SAIR" = return()
        loop x = do
            putStrLn(x)
            repetirAteSair
        alto t = map (toUpper) t

--(12)Crie um jogo interativo chamado adivinharNumero :: Int -> IO (). A função recebe um número secreto como parâmetro 
--(ex: adivinharNumero 42). O programa deve pedir palpites ao usuário dentro de um loop de IO. A cada palpite errado, 
--o programa deve responder se o número secreto é "Maior" ou "Menor" do que o palpite fornecido. Quando o usuário acertar, 
--o programa exibe "Parabéns, você acertou!" e encerra.

adivinharNumero :: Int -> IO ()
adivinharNumero numsec = do
    tentativa <- getLine
    verificar numsec (read tentativa :: Int)
    where
        verificar a b
            |a > b = do
                putStrLn("Maior")
                adivinharNumero a
            |a < b = do
                putStrLn("Menor")
                adivinharNumero a
            |otherwise = do
                putStrLn("Parábens, você acertou!")

