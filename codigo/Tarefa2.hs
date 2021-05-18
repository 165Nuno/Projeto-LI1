{- | __Tarefa :__ Tarefa2
     __Descrição :__ Criar jogadas
     __Grupo233:__ Nuno Costa "a96897@alunos.uminho.pt"
                   Ivo Ribeiro "a96726@alunos.uminho.pt"

= Introdução

A Tarefa 2 foi a segunda tarefa que começamos a elaborar, e que consistia em implementar jogadas aos jogadores de modo 
que fosse fácil mover os jogadores.

= Objetivos
O nosso objetivo desta Tarefa baseou-se em criar metodos que permitissem jogar em si o jogo . Comecamos por defenir uma das funções mais importantes 
do jogo que é a mover que permite mover com base na Orientação tanto os fantasmas quer o Pacman.

De seguida, pensamos em criar funções onde houvesse colisão tanto do ghost com o pacman e vice-versa. Isto permitia que dadas algumas condições, os states 
quer dos jogadores quer do Pacman. Para além disso, criamos funções tais teletunel que permite o pacman usufruir do tunel(o ghost não tem esta vantagem) e 
a função do UpdateMaze , que serve para dar update ao maze depois da interações do Pacman e dos Ghosts (no primeiro caso a comida , quer grande quer pequena, passa para VAZIO, 
já no segundo caso, a comida mantem-se no mapa já que os ghosts não interferem.)

Por fim, temos a função play, que com o inicio da parte 2 tivemos de a alterar, permite jogar tantos os Ghosts tanto os Pacmans.

= Discussão e conclusão

Esta Tarefa foi na nossa opinião mais desafiante e que nos obrigou a pensar fora da caixa , já que fez pensar como tudo se iria juntar e como realmente o jogo funciona.
Concluindo, os resultados que obtivemos foram os esperados , uma vez que , com esta Tarefa já conseguimos implementar jogadas , estando cada vez mais perto do jogo original.

-}



module Tarefa2 where
import Types

import FileUtils


-- | Exemplo de jogadores
js = [jogador1,jogador2]
-- | Exemplo jogador 1
jogador1= (Pacman (PacState (1,(7,10),1,R,0,1) 0 Open Normal))
-- | Exemplo jogador 2
jogador2 = (Ghost (GhoState (0,(5,12),1,R,0,1) Alive))


-- | __Função que dado um Pacman/Ghost e uma Orientação , o Pacman dependendo da sua Orientação e da Orientação dado reage e move-se__



mover :: Player -> Orientation -> Maze -> Player
mover (Pacman(PacState (x,(a,b),z,t,h,l) q c d)) o (lb:ls) | b == 0 && o == L = teletunel (abreboca (Pacman(PacState (x,(a,b),z,t,h,l) q c d))) (lb:ls)
                                                           | b == (length lb)-1  && o == R = teletunel (abreboca (Pacman(PacState (x,(a,b),z,t,h,l) q c d))) (lb:ls)
                                                           | o == L && t == L = abreboca (Pacman(PacState (x,(a,(b-1)),z,t,h,l) q c d)) 
                                                           | o == R && t == R = abreboca (Pacman(PacState (x,(a,(b+1)),z,t,h,l) q c d)) 
                                                           | o == U && t == U = abreboca (Pacman(PacState (x,((a-1),b),z,t,h,l) q c d)) 
                                                           | o == D && t == D = abreboca (Pacman(PacState (x,((a+1),b),z,t,h,l) q c d)) 
                                                           |otherwise = Pacman (PacState (x,(a,b),z,o,h,l) q c d )

mover (Ghost(GhoState (x,(a,b),z,t,h,l) d)) o (lb:ls) | b == 0 && o == L = teletunel (Ghost(GhoState(x,(a,b),z,t,h,l) d)) (lb:ls)
                                                      | b == (length lb)-1  && o == R = teletunel (Ghost(GhoState(x,(a,b),z,t,h,l) d)) (lb:ls)
                                                      | o == L && t == L = if obterpeca (lb:ls) (a,b-1)  == Wall then (Ghost(GhoState (x,(a,b),z,t,h,l) d)) else Ghost(GhoState (x,(a,(b-1)),z,t,h,l) d) 
                                                      | o == R && t == R = if obterpeca (lb:ls) (a,b+1)  == Wall then (Ghost(GhoState (x,(a,b),z,t,h,l) d)) else Ghost(GhoState (x,(a,(b+1)),z,t,h,l) d) 
                                                      | o == U && t == U = if obterpeca (lb:ls) (a-1,b)  == Wall then (Ghost(GhoState (x,(a,b),z,t,h,l) d)) else Ghost(GhoState (x,((a-1),b),z,t,h,l) d) 
                                                      | o == D && t == D = if obterpeca (lb:ls) (a+1,b)  == Wall then (Ghost(GhoState (x,(a,b),z,t,h,l) d)) else Ghost(GhoState (x,((a+1),b),z,t,h,l) d) 
                                                      |otherwise = Ghost(GhoState (x,(a,b),z,o,h,l) d)
                                                  
-- | __Função que faz o pacman reagir quando se apresenta nas mesmas coordenadas que Ghost , de seguida sofre as suas alterações__
pg :: Maze -> Player -> [Player] -> Player
pg l (Pacman (PacState (x,(a,b),z,t,h,r) q c d )) [] = Pacman (PacState (x,(a,b),z,t,h,r) q c d)
pg l (Pacman (PacState (x,(a,b),z,t,h,r) q c d)) ((Ghost (GhoState (i,(a2,b2),v,o,p,k) d1)) : xs)| a == a2 && b == b2 && r <= 0 && d1 == Alive = pg l (Pacman (PacState (x,(a,b),z,Null,h,0) q c Dying)) xs
                                                                                             | a == a2 && b == b2 && r > 0 && d1 == Alive = pg l (Pacman (PacState (x,(a,b),z,t,h,(r-1)) q c d)) xs
                                                                                             | a == a2 && b == b2 && d1 == Dead && d == Mega = pg l (Pacman (PacState (x,(a,b),z,t,(h+10),r) q c Mega)) xs
                                                                                             | d == Dying = error "GAME OVER : PACMAN PERDEU AS VIDAS"
                                                                                             |otherwise = (Pacman(PacState (x,(a,b),z,t,h,r) q c d ))
-- | __Função que faz o Ghost reagir ao Pacman e sofrer as suas alterações__ 
ghostsvspac:: Maze -> Player -> [Player] -> Int -> [Player]
ghostsvspac l (Pacman (PacState (x,(a,b),z,t,h,r) q c d )) [] id = [Pacman (PacState (x,(a,b),z,t,h,r) q c d)]
ghostsvspac l (Pacman (PacState (x,(a,b),z,t,h,r) q c d )) ((Ghost (GhoState (i,(a2,b2),v,o,p,k) d1)) : xs) id | id == i && a == a2 && b == b2 && d1 == Dead && d == Mega = teleportfantasmas (Ghost (GhoState (i,(a2,b2),v,o,p,k) d1):xs) l (Pacman (PacState (x,(a,b),z,t,h,r) q c d ))
                                                                                                              | id == i && a == a2 && b == b2 && d1 == Alive = (Ghost (GhoState (i,(a2,b2),v,o,p,k) d1)) : ghostsvspac l (Pacman (PacState (x,(a,b),z,t,h,(r-1)) q c d )) xs id
                                                                                                              |otherwise = (Ghost (GhoState (i,(a2,b2),v,o,p,k) d1)) : ghostsvspac l (Pacman (PacState (x,(a,b),z,t,h,r) q c d )) xs id
-- | __Função que realiza o teletransporte dos Fantasmas para a casa dos fantasmas depois de serem devorados pelo o Pacman em Estada Mega__
teleportfantasmas :: [Player] -> Maze -> Player -> [Player]
teleportfantasmas [] (h:t) p = [p]
teleportfantasmas ((Ghost (GhoState (x,(a,b),v,o,p,k) d1)) : xs) (h:t) (Pacman (PacState (x1,(a1,b1),v1,o1,p1,k1) q2 c2 d2)) | a == a1 && b == b1 && d1 == Dead && d2 == Mega = setPlayerCoords (Ghost (GhoState (x,(a,b),v,o,p,k) Alive)) ((div (length (h:t)) 2),(div (length h) 2)) : teleportfantasmas xs (h:t) (Pacman (PacState (x1,(a1,b1),v1,o1,p1,k1) q2 c2 d2))
                                                                                                                          | otherwise = (Ghost (GhoState (x,(a,b),v,o,p,k) d1 )) : teleportfantasmas xs (h:t) (Pacman (PacState (x1,(a1,b1),v1,o1,p1,k1) q2 c2 d2))

-- | __Função em que dado um Pacman e um Maze , verificamos qual a peca que corresponde as coordenadas que o Pacman está e logo de seguida, ele reage a esse acontecimento. Nesta função, considero que o Pacman move-se para as paredes mas consoante a sua orientação ele volta atras , evitando deste modo que consiga deslocar-se entre paredes.__
funcaocolisao :: Player -> Maze -> Player 
funcaocolisao (Pacman (PacState (x,(a,b),v,o,p,k) q1 c1 d1)) (h:t) | obterpeca (h:t) (a,b) == Food Little = Pacman (PacState (x,(a,b),v,o,(p+1),k) q1 c1 d1)
                                                                   | obterpeca (h:t) (a,b) == Food Big = Pacman (PacState (x,(a,b),v,o,(p+5),k) 10 c1 Mega ) 
                                                                   | obterpeca (h:t) (a,b) == Wall && o == L = Pacman (PacState (x,(a,(b+1)),v,o,p,k) q1 c1 d1)
                                                                   | obterpeca (h:t) (a,b) == Wall && o == R = Pacman (PacState (x,(a,(b-1)),v,o,p,k) q1 c1 d1)
                                                                   | obterpeca (h:t) (a,b) == Wall && o == U = Pacman (PacState (x,((a+1),b),v,o,p,k) q1 c1 d1)
                                                                   | obterpeca (h:t) (a,b) == Wall && o == D = Pacman (PacState (x,((a-1),b),v,o,p,k) q1 c1 d1)
                                                                   | otherwise = Pacman (PacState (x,(a,b),v,o,p,k) q1 c1 d1)

                   
-- | __Função que dá update ao Maze depois do movimento do Pacman__
updateMazezinho :: Player -> Maze -> Maze
updateMazezinho (Pacman (PacState (x,(a,b),v,o,p,k) q1 c1 d1 )) (h:t) | obterpeca (h:t) (a,b) == Food Little = replaceElemInMaze (a,b) Empty (h:t)
                                                                      | obterpeca (h:t) (a,b) == Food Big = replaceElemInMaze (a,b) Empty (h:t)
                                                                      | obterpeca (h:t) (a,b) == Empty = replaceElemInMaze (a,b) Empty (h:t)
                                                                      | obterpeca (h:t) (a,b) /= Wall = replaceElemInMaze (a,b) Empty (h:t)
                                                                      | obterpeca (h:t) (a,b) == Wall = replaceElemInMaze (a,b) Wall (h:t)
                                                                      | otherwise = replaceElemInMaze (a,b) Empty (h:t)

-- | __Nesta Função o Pacman reage aos tuneis , saindo sempre na posição à frente__
teletunel :: Player -> Maze -> Player
teletunel (Pacman(PacState (x,(a,b),v,o,p,k) q1 c1 d1)) (h:t) | b == 0  = (Pacman(PacState(x,(a,(length h)-1),v,o,p,k) q1 c1 d1)) 
                                                              | b == (length h)-1 = (Pacman(PacState(x,(a,0),v,o,p,k) q1 c1 d1)) 
                                                              | otherwise = (Pacman(PacState(x,(a,b),v,o,p,k) q1 c1 d1))

{-teletunel (Ghost(GhoState (x,(a,b),z,t,h,l) d)) o (lb:ls)  | b == 0  = (Ghost(GhoState (x,(a,(length h)-1),z,t,h,l) d)) o (lb:ls) 
                                                           | b == (length h)-1 = (Ghost(GhoState (x,(a,0),z,t,h,l) d)) o 
                                                           | otherwise = (Ghost(GhoState (x,(a,b),z,t,h,l) d)) o (lb:ls) 
-}

-- | __Nesta função todos os Fantasmas passam para Dead logo após o Pacman entrar em Mega__
entrarmegatime :: Player -> [Player] -> [Player]
entramegatime _ [] = []
entrarmegatime (Pacman(PacState(x,(a,b),v,o,p,k) q1 c1 d1)) ((Ghost (GhoState (x2,(a2,b2),v2,o2,p2,k2) Alive)) : xs) | d1 == Mega = (Ghost (GhoState (x2,(a2,b2),(v2/ 2),o2,p2,k2) Dead)) : entrarmegatime (Pacman(PacState(x,(a,b),v,o,p,k) q1 c1 d1)) xs
                                                                                                                    | otherwise = (Ghost (GhoState (x2,(a2,b2),v2,o2,p2,k2) Alive)) : entrarmegatime (Pacman(PacState(x,(a,b),v,o,p,k) q1 c1 d1)) xs



-- | Função final onde são defenidos os casos especiais
-- | = __Exemplo:__
-- | > play (Move 0 L) (State (generateMaze 20 20 1) [Pacman (PacState (0,(12,10),1,L,0,1) 10 Open Normal),Ghost (GhoState (1,(3,8 ),1,U,0,3) Alive),Ghost (GhoState (2,(5,10),1,U,0,3) Alive)] 9)
play :: Play -> State -> State
play (Move id o) (State l x n) | encontraid x id /= encontrapacman x = let ghost = encontraid x id
                                                                           ghost1 = mover ghost o l
                                                                           pacman = encontrapacman x 
                                                                           idpacman = getPlayerID pacman
                                                                           l1 = semjoga (semjoga x id) idpacman
                                                                           l2 = ghost1 : l1
                                                                           l3 = ghostsvspac l pacman l2 id
                                                                       in State l l3 n 

                               |otherwise = let pacman = encontraid x id
                                                lghosts = encontraghots x
                                                pacmanatua = pg l (funcaocolisao (mover pacman o l) l) lghosts
                                                lba = updateMazezinho pacman l 
                                                lpa = teleportfantasmas lghosts lba pacmanatua  
                                            in State lba lpa n





