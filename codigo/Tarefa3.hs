{- | Tarefa : Tarefa3
     Descrição : Compactar Labirinto
     Grupo233 : Nuno Costa "a96897@alunos.uminho.pt"
                Ivo Ribeiro "a96726@alunos.uminho.pt"

= Introdução
Nesta Tarefa foi no pedido, que fosse gerado a partir de um Maze uma sequencia de Intruções de modo que fosse fácil o manuseamento de muita informação e de compactar.

= Objetivos 
Numa fase inicial, tentamos entender como iriamos compactar a informação dada pelo o Maze, começamos então por criar uma função que nos devolve um [(Int,Piece)] 
,essa função foi baseada numa função que já tinha feito na 50 questões. Continuando a tarefa, criamos 2 funções que uma permite-nos saber o indice que um dado
corredor aparece num Maze e a outra permite transformar uma maze em uma Intructions. Por fim , temos a __compactMaze__ que dado um labirinto retorna uma sequência 
de Instruções.

= Conlusão 
Concluindo,conseguimos completar a tarefa conforme o esperado pelo o nosso grupo.

-}


module Tarefa3 where
import Tarefa1
import Types
import FileUtils

-- | __exemplo de um Maze__
exemplo :: Maze
exemplo = [[Wall,Wall,Wall,Wall,Wall],[Food Little,Food Big,Food Little,Food Big,Food Big],[Wall,Wall,Wall,Wall,Wall]]

exemplo1 :: Maze
exemplo1 = [[Wall,Wall,Wall,Wall,Wall],[Food Big,Food Big,Food Big,Food Big,Food Big],[Wall,Wall,Wall,Wall,Wall]]

exemplo3 :: Maze
exemplo3 = [[Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall],[Wall,Wall,Wall,Wall,Wall]]

-- | __Função que transforma o corredor no tipo (Int,Piece)__
auxgrupa :: Int -> Corridor -> [(Int,Piece)]
auxgrupa i [x] = [(i,x)]
auxgrupa i (x:y:xs) = if x == y
                    then auxgrupa (i+1) (x:xs)
                    else (i,x) : auxgrupa 1 (y:xs)
-- | __Função que devolve o indice da primeira vez que um corredor aparece num Maze__
er :: Int -> Maze -> Corridor -> Int
er c [] l = error "erro erro"
er c (h:t) l | h == l = c
             | otherwise = er (c+1) t l 
-- | __Função que dado dois mazes identicos , transforma o maze num tipo Instruction__
er2 :: Int -> Maze -> Maze -> Instructions
er2 c []  m = []
er2 c (h:t) m  | c == er 0 m h =  Instruct (auxgrupa 1 h) : er2 (c+1) t m
            | otherwise = Repeat (er 0 m h): er2 (c+1) t m
-- | __Função final que dado um Maze devolve-me Instructions__
-- | = __Exemplo:__
-- | > compactMaze [[Wall,Wall,Wall,Wall,Wall],[Food Little,Food Big,Food Little,Food Big,Food Big],[Wall,Wall,Wall,Wall,Wall]]
compactMaze :: Maze -> Instructions
compactMaze m = er2 0 m m 