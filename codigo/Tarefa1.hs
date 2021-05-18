{- | __Tarefa:__ Tarefa1
     __Descrição :__ Criar os labirintos
     __Grupo233 :__ Nuno Costa "a96897@alunos.uminho.pt"
                    Ivo Ribeiro "a96726@alunos.uminho.pt"

= Introdução 
Esta era uma tarefa no qual nos foi proposto a realização de labirintos que seria usados para jogarmos Pacman.

= Objetivos
No inicio desta tarefa estavamos um pouco perdidos como começar , mas depois de partilhamos ideias e de conversas , decidimos dividir 
esta Tarefa em partes . Na primeira parte começamos por defenir aspetos básicos como defenir as funções (__convertePeca__,__geraParede__,__geraLittleFood__,etc).
Estas funções simples foram essenciais para as funções que elebramos posteriormente.Na 2 parte, resolvermos desenvolver a parte dos tuneis no labirinto,
para isto sabiamos que os tuneis era diferente caso a altura do labirinto fosse par ou impar , e para isso utilizamos funções auxiliares que tornaram mais simples.
Por fim, abordamos a parte da casa dos fantasmas e como poderiamos implementa-la nesta tarefa. Chegamos a defenir para todos os casos possiveis de largura e altura
(ou seja, par ou impar)

== Discussão e conclusão

Ao observar o que obtemos , ficamos satisfeitos com o resultado , uma vez que, foi a nossa primeira tarefa realizada para o TP.
Concluindo , podemos indicar que a tarefa foi um sucesso tendo em conta os objetivos do nosso grupo.


-}

module Tarefa1 where

import System.Random

import Types


-- | __Função onde é dado uma seed e que é retornado uma lista__

geraAleatorios :: Int -> Int -> [Int]
geraAleatorios n seed = let gen = mkStdGen seed 
                        in take n $ randomRs (0,99) gen 
                        
                        


-- | __Função que dado uma seed retorna um número criado aleatoriamente__
nrAleatorio :: Int -> Int
nrAleatorio seed = head $ geraAleatorios 1 seed


-- | __Função que converte uma lista numa lista de listas com o tamanho de n__

subLista :: Int -> [a] -> [[a]]
subLista _ [] = []
subLista n l = take n l: subLista n (drop n l)


-- | __Converte um numero Inteiro em uma Peca__
-- 3 <=> Comida Granfre
-- 0 <= n < 7 <=> Comida Pequena
-- 7 < n <= 9 <=> Parede
--
convertePeca :: Int -> Piece
convertePeca n | n == 3 = Food Big
               | 0 <= n && n < 70 = Food Little
               | 70 <= n && n <=99 = Wall
               

-- | __Função que dado um Inteiro gera um corredor de Paredes do tamanho n__
geraParede :: Int -> Corridor
geraParede 0 = []
geraParede n = convertePeca 88 : geraParede (n-1)

-- | __Função que dado um Inteiro gera um corredor de LittleFood do tamanho n__
geraLittleFood  :: Int -> Corridor
geraLittleFood 0 = []
geraLittleFood n = convertePeca 66 : geraLittleFood (n-1)



-- | __Casa Fantasmas Par__
casadosfantamaspar :: Maze
casadosfantamaspar = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                     [Empty,Wall,Wall,Wall,Empty,Empty,Wall,Wall,Wall,Empty],
                     [Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],
                     [Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty],
                     [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]

-- | __Casa Fantasmas Impar__
casadosfantamasimpar :: Maze
casadosfantamasimpar = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],
                     [Empty,Wall,Wall,Wall,Empty,Empty,Empty,Wall,Wall,Wall,Empty],
                     [Empty,Wall,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Wall,Empty],
                     [Empty,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Empty],
                     [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]


-- | __Função que dado um Inteiro e um Maze , ela calcula se a altura do Maze é impar ou par__                
encontrarlinha :: Int -> Int -> Maze -> Maze
encontrarlinha n k (h:t) | mod n 2 == 0  &&  mod k 2 == 0 = auxparfantasmas 0  k  (h:t) casadosfantamaspar
                         | mod n 2 == 0 = auxparimparfantasmas 0  k  (h:t) casadosfantamaspar
                         | mod n 2 == 1 && mod k 2 == 0 = auximparparfantasmas 0 k (h:t) casadosfantamasimpar
                         |otherwise = auximparfantasmas 0 k (h:t) casadosfantamasimpar
-- | __Função que dado 2 Inteiros , um Maze e uma Casa de Fantasmas Par , ela calcula as linhas que irão ser afetadas__
auxparfantasmas :: Int -> Int -> Maze -> Maze -> Maze
auxparfantasmas _ _ [] _= []
auxparfantasmas _ _ (h:t) [] = (h:t)
auxparfantasmas c n (h:t) (x:xs) | c < (div n 2) -3 = h : auxparfantasmas (c+1) n t (x:xs)
                                 | c == (div n 2) -2 = (criarcasafantasmas 0 (length(h)) h x) : (auxparfantasmas (c+1) n t xs)
                                 | c == (div n 2)-3 = (criarcasafantasmas 0 (length(h)) h x) : (auxparfantasmas (c+1) n t xs)
                                 | c == (div n 2)-1= (criarcasafantasmas 0 (length(h)) h x) : (auxparfantasmas (c+1) n t xs)
                                 | c == (div n 2) = (criarcasafantasmas 0 (length(h)) h x): (auxparfantasmas (c+1) n t xs)
                                 | c == (div n 2)+1 = (criarcasafantasmas 0 (length(h)) h x) : (auxparfantasmas (c+1) n t xs)
                                 |otherwise = h : (auxparfantasmas (c+1) n t [])

-- | __Função que dado 2 Inteiros , um Maze e uma Casa de Fantasmas Impar , ela calcula as linhas que irão ser afetadas__
auximparfantasmas :: Int -> Int -> Maze -> Maze -> Maze
auximparfantasmas _ _ [] _= []
auximparfantasmas _ _ (h:t) [] = (h:t)
auximparfantasmas c n (h:t) (x:xs) | c < (div n 2) -2 = h : auximparfantasmas (c+1) n t (x:xs)
                                 | c == (div n 2)-2 = (criarcasaifantasmas 0 (length(h)) h x) : (auximparfantasmas (c+1) n t xs)
                                 | c == (div n 2)-1 = (criarcasaifantasmas 0 (length(h)) h x) : (auximparfantasmas (c+1) n t xs)
                                 | c == (div n 2) = (criarcasaifantasmas 0 (length(h)) h x) : (auximparfantasmas (c+1) n t xs)
                                 | c == (div n 2)+1 = (criarcasaifantasmas 0 (length(h)) h x): (auximparfantasmas (c+1) n t xs)
                                 | c == (div n 2)+2 = (criarcasaifantasmas 0 (length(h)) h x) : (auximparfantasmas (c+1) n t xs)
                                 |otherwise = h : (auximparfantasmas (c+1) n t [])

-- | __Função que dado 2 Inteiros, um Corredor e um Corredor da Casa dos Fantasmas Impar, ele substituiu os elementos do Corredor do Maze que darão ligar aos elementos do Corredor da Casa dos Fantasmas Impar__
criarcasaifantasmas :: Int -> Int -> Corridor -> Corridor -> Corridor
criarcasaifantasmas _ _[] _ = []
criarcasaifantasmas c  k (h:t) (x:xs)| c < (div k 2) -5 = h : (criarcasaifantasmas (c+1) k t (x:xs))
                                     | c >= (div k 2) -5 && c <= (div k 2) +4 = x : (criarcasaifantasmas (c+1) k t xs)
                                     | otherwise = (x:xs) ++ t

-- | __Função que dado 2 Inteiros, um Corredor e um Corredor da Casa dos Fantasmas Par, ele substituiu os elementos do Corredor do Maze que darão ligar aos elementos do Corredor da Casa dos Fantasmas Par__
criarcasafantasmas :: Int -> Int -> Corridor -> Corridor -> Corridor
criarcasafantasmas _ _[] _ = []
criarcasafantasmas c  k (h:t) (x:xs) | c < (div k 2) -5 = h : (criarcasafantasmas (c+1) k t (x:xs))
                                     | c >= (div k 2) -5 && c < (div k 2) + 4 = x : (criarcasafantasmas (c+1) k t xs)
                                     | otherwise = (x:xs) ++ t

-- | __Função que dados dimensoes par e impar e dois Mazes , devolve-nos um Maze__
auxparimparfantasmas :: Int -> Int -> Maze -> Maze -> Maze
auxparimparfantasmas _ _ [] _= []
auxparimparfantasmas _ _ (h:t) [] = (h:t)
auxparimparfantasmas c n (h:t) (x:xs) | c < (div n 2) -2 = h : auxparimparfantasmas (c+1) n t (x:xs)
                                 | c == (div n 2) -2 = (criarcasafantasmas 0 (length(h)) h x) : (auxparimparfantasmas (c+1) n t xs)
                                 | c == (div n 2)+2 = (criarcasafantasmas 0 (length(h)) h x) : (auxparimparfantasmas (c+1) n t xs)
                                 | c == (div n 2)-1= (criarcasafantasmas 0 (length(h)) h x) : (auxparimparfantasmas (c+1) n t xs)
                                 | c == (div n 2) = (criarcasafantasmas 0 (length(h)) h x): (auxparimparfantasmas (c+1) n t xs)
                                 | c == (div n 2)+1 = (criarcasafantasmas 0 (length(h)) h x) : (auxparimparfantasmas (c+1) n t xs)
                                 |otherwise = h : (auxparimparfantasmas (c+1) n t [])
-- | __Função que dados dimensoes impares e pares , e dois Mazes , devolve-nos um Maze__                               
auximparparfantasmas :: Int -> Int -> Maze -> Maze -> Maze
auximparparfantasmas _ _ [] _= []
auximparparfantasmas _ _ (h:t) [] = (h:t)
auximparparfantasmas c n (h:t) (x:xs) | c < (div n 2) -3 = h : auximparparfantasmas (c+1) n t (x:xs)
                                 | c == (div n 2)-3 = (criarcasaifantasmas 0 (length(h)) h x) : (auximparparfantasmas (c+1) n t xs)
                                 | c == (div n 2)-1 = (criarcasaifantasmas 0 (length(h)) h x) : (auximparparfantasmas (c+1) n t xs)
                                 | c == (div n 2) = (criarcasaifantasmas 0 (length(h)) h x) : (auximparparfantasmas (c+1) n t xs)
                                 | c == (div n 2)+1 = (criarcasaifantasmas 0 (length(h)) h x): (auximparparfantasmas (c+1) n t xs)
                                 | c == (div n 2)-2 = (criarcasaifantasmas 0 (length(h)) h x) : (auximparparfantasmas (c+1) n t xs)
                                 |otherwise = h : (auximparparfantasmas (c+1) n t [])


-- | __Função que converte uma lista de inteiros num Corredor__
converteCorredor :: [Int] -> Corridor
converteCorredor [] = []
converteCorredor (x:xs) = convertePeca x : converteCorredor xs



-- | __Função que cria as Paredes Laterais__
criaParedeLado :: Corridor -> Corridor
criaParedeLado l = [Wall] ++ (tail(init l) ++ [Wall])

-- | __Função que dado um Inteiro e um Labirinto, ela calcula se a altura é par ou impar__
tunelpar :: Int -> Maze-> Maze
tunelpar n [] = []
tunelpar n (h:t)
                |mod n 2 == 0 = auxpar 0 n (h:t)
                |otherwise = auximpar 0 n (h:t)
-- | __Função que dado 2 inteiros e um Maze , ela calcula os corredores que serão afetados pela implementação do tunel caso a altura seja Par__
auxpar :: Int -> Int -> Maze -> Maze
auxpar _ _ [] = []
auxpar c n (h:t) | c == div n 2 = ([Empty] ++ ((tail(init (h))++ [Empty]))): (auxpar (c+1) n t)
                 | c == (div n 2) -1 = ([Empty] ++ ((tail(init (h))++ [Empty]))) : (auxpar (c+1) n t)
                 | otherwise = [criaParedeLado h] ++ (auxpar (c+1) n t)

-- | __Função que dado 2 inteiros e um Maze , ela calcula os corredores que serão afetados pela implementação do tunel caso a altura seja Impar__
auximpar :: Int -> Int -> Maze -> Maze
auximpar _ _ [] = []
auximpar c n (h:t) | c == div n 2 = ([Empty] ++ ((tail(init (h))++ [Empty]))): (auximpar (c+1) n t)
                   |otherwise = [criaParedeLado h] ++ (auximpar (c+1) n t)
-- | __Função que recebe 2 inteiros e um Labirinto , que retorna um Labirinto com as paredes em cima e em baixo__
mixada :: Int -> Int -> Maze -> Maze
mixada n x (h:t) = [geraParede (n)] ++ (tail(init((tunelpar x (h:t)))) ++ [geraParede (n)])




-- | __Função que transforma uma lista de inteiros num Labirinto__
converteLabirinto :: [[Int]] -> Maze
converteLabirinto [] = []
converteLabirinto (x:xs) = converteCorredor x : converteLabirinto xs



-- | __Função que junta as funções anteriormente expostas numa em que retorna o labirinto final__
-- | = __Exemplo:__
-- | > generateMaze 30 30 233
generateMaze :: Int -> Int -> Int -> Maze
generateMaze x y s =
                 let random_nrs = geraAleatorios (x*y) s
                 in mixada x y $ encontrarlinha x y $ converteLabirinto $ subLista x random_nrs

-- | __Função final que imprime neste caso o labirinto final__
-- = __Exemplo:__
-- > imprimeLabirinto (generateMaze 30 30 233)
imprimeLabirinto :: Maze -> IO () 
imprimeLabirinto l = do putStrLn ( printMaze ( l ))