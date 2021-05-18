{- | Tarefa : Tarefa5
     Descrição : Criar o Movimento dos Ghosts
     Grupo233: Nuno Costa "a96897@alunos.uminho.pt"
              Ivo Ribeiro "a96726@alunos.uminho.pt"

= Introdução
 
 A tarefa 5 foi nos pedido para realizar um mecanismo em que os fantasmas se moviam sozinhos, isto é, através da utilização manual do Pacman , os Ghosta assumiam a 
 melhor alternativa para perseguir o Pacman, tal como o no jogo Original,para tal criamos alguma funções que permitissem isso e alteramos algumas partes da Tarefa 2.

= Objetivos

 O nosso objetivo neste Tarefa 5 passou por tentar implementar um metodo de calcular a melhor maneira dos Ghosts encontrarem o Pacman . Tentamos ao longo desta tarefa 
 usar codigo que fosse simples entender e de explicar. Este mecanismo podemos dividi-lo em 2 partes: 

 A primeira é quando os fantasmas se encontram mortos, eles tem a tendência de seguir sempre na mesma orientação que têm , até encontrarem uma parede . 
 Nestes casos, eles rodam para a direita (obtem uma nova rotacao) e voltam a seguir a sua direção nova. Isto leva o fantasma a criar um caminho que evite o pacman meio que em forma de ciclo.

 O outro mecanismos é baseado por onde o Pacman está em relação ao Ghost, por exemplo se o Pacman estiver a esquerda do Ghost, este assumirá que deve ir para L , e assim sucessivamente 
 para as outra orientações. Embora não seja um algoritmo de procura de caminho mais curto , a ideia é que o Ghost se movimente sempre em direção ao Pacman.

= Discussão e conclusão

Nesta Tarefa , conseguiu por nos a prova de como seria possivel arranjar forma de os Ghosts irem atras de o Pacman, de uma forma simples e que fosse facil de entender.
Conlcuindo, os resultados que obtemos são o que estavamos á espera, uma vez que, eles tomam as direções que devem tomar mas os ghosts apesar de em certas vezes, obtemos movimentos
indesejados.


-}

module Tarefa5 where 

import Types
import Tarefa1

-- | __Exemplo de lista de jogadores__
jogadores :: [Player]
jogadores = [Pacman (PacState (0,(12,10),1,L,0,1) 10 Open Normal),
            Ghost (GhoState (1,(3,8 ),1,U,0,3)       Alive),
            Ghost (GhoState (2,(5,10),1,U,0,3)       Alive)]


-- | __Exemplo de Maze__
g = generateMaze 20 20 1

-- | __Exemplo de um State__
e = State g jogadores 9






-- | __Função que permite a rotação para a direita dada uma Orientation__
rotacao :: Orientation -> Orientation -- fazer como o quinhe disse
rotacao o | o == R = D 
          | o == D = L
          | o == L = U
          | o == U = R
-- | __Função que quando os fantasmas estão mortos e encontra uma parede á sua frente , eles executam uma rotação em 90 graus para a direita__
mortos :: Player -> Maze -> Player
mortos (Ghost (GhoState (id,(x,y),velocity,orientation,points,lives) d )) m
          | obterpeca m (x+1,y) == Wall && orientation == D = (Ghost (GhoState (id,(x,y),velocity,(rotacao orientation),points,lives) d ))
          | obterpeca m (x-1,y) == Wall && orientation == U = (Ghost (GhoState (id,(x,y),velocity,(rotacao orientation),points,lives) d ))
          | obterpeca m (x,y+1) == Wall && orientation == R = (Ghost (GhoState (id,(x,y),velocity,(rotacao orientation),points,lives) d ))
          | obterpeca m (x,y-1) == Wall && orientation == L = (Ghost (GhoState (id,(x,y),velocity,(rotacao orientation),points,lives) d ))

-- | __Função que quando os fantasmas estão__
vivaos :: Player -> Player -> Maze -> Player
vivaos (Pacman (PacState (id,  (x,y), velocity, orientation, points, lives) n i g )) (Ghost (GhoState (id9,(x9,y9),velocity9,orientation9,points9,lives9) d )) m 
                                | x < x9 && obterpeca m (x-1,y) /= Wall = Ghost (GhoState (id9,(x9,y9),velocity9,U,points9,lives9) d )
                                | x > x9 && obterpeca m (x+1,y) /= Wall = Ghost (GhoState (id9,(x9,y9),velocity9,D,points9,lives9) d )
                                | y < y9 && obterpeca m (x,y-1) /= Wall = Ghost (GhoState (id9,(x9,y9),velocity9,L,points9,lives9) d )
                                | y > y9 && obterpeca m (x,y+1) /= Wall = Ghost (GhoState (id9,(x9,y9),velocity9,R,points9,lives9) d )
                                | obterpeca m (x-1,y) == Wall && obterpeca m (x+1,y) == Wall && obterpeca m (x,y+1) == Wall = Ghost (GhoState (id9,(x9,y9),velocity9,L,points9,lives9) d )
                                | obterpeca m (x-1,y) == Wall && obterpeca m (x+1,y) == Wall && obterpeca m (x,y-1) == Wall = Ghost (GhoState (id9,(x9,y9),velocity9,R,points9,lives9) d )
                                | obterpeca m (x-1,y) == Wall && obterpeca m (x,y-1) == Wall && obterpeca m (x,y+1) == Wall = Ghost (GhoState (id9,(x9,y9),velocity9,U,points9,lives9) d )
                                | obterpeca m (x+1,y) == Wall && obterpeca m (x,y-1) == Wall && obterpeca m (x,y+1) == Wall = Ghost (GhoState (id9,(x9,y9),velocity9,D,points9,lives9) d )
                                | otherwise = (Ghost (GhoState (id9,(x9,y9),velocity9,orientation9,points9,lives9) d ))


-- | __Função quando os Ghosts estão vivos e andam a procura do Pacman__
chaseMode :: State -> Int -> Play -- vivo
chaseMode (State l x n) k = Move k (getPlayerOrientation(vivaos (encontrapacman x) (encontraid x k) l))

-- | __Função quando os Ghosts estão mortos e tem de fugir do Pacman__
scatterMode :: State -> Int -> Play -- morto
scatterMode (State l x n) k = Move k (getPlayerOrientation(mortos (encontraid x k) l)) 

-- | __Função que dado um Ghost devolve o seu Modo__
getGhostMode :: Player -> GhostMode
getGhostMode (Ghost (GhoState (a,b,c,d,e,f) m ))= m

-- | Função final da Tarefa 5

-- | = __Exemplo:__
-- | ghostPlay (State (generateMaze 20 20 1) [Pacman (PacState (0,(12,10),1,L,0,1) 10 Open Normal),Ghost (GhoState (1,(3,8 ),1,U,0,3) Alive),Ghost (GhoState (2,(5,10),1,U,0,3) Alive)] 9)
ghostPlay :: State -> [Play]
ghostPlay (State m l x) 
                  | length l == 1 = []
                  | (getGhostMode (head (encontraghots l))) == Alive = chaseMode (State m l x) (getPlayerID (head(encontraghots l))) : ghostPlay (State m ((tail(encontraghots l))++[encontrapacman l]) x)
                  | (getGhostMode (head (encontraghots l))) == Dead = scatterMode (State m l x) (getPlayerID (head(encontraghots l))) : ghostPlay (State m ((tail(encontraghots l))++[encontrapacman l]) x)

