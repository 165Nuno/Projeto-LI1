{- | Tarefa: Tarefa4
     Descrição : Criar a passagem de tempo
     Grupo233 : Nuno Costa "a96897@alunos.uminho.pt"
                Ivo Ribeiro "a96726@alunos.uminho.pt"

= Introdução
Nesta Tarefa 4 foi-nos pedido realização de uma tarefa onde implementassemos a passagem de tempo ao jogo , isto é , criamos
um conjunto de funçoes que quando chamadas devolve-se o estado do jogo atualizado.

= Objetivos
 O Objetivo nesta Tarefa passou inicialmente por fazer alterações á tarefa 2 , onde introduzimos o mecanismo para os Ghosts conseguirem
 deslocar-se , introduzimos também o Movimento da boca do Pacman que a cada movimento abre e fecha sucessiavemnte. Na Tarefa 4 , baseamos-nos em criar uma função que permita reduzir o tempo de Mega em 0.25 
 o Pacman estiver e também de o passar para o Estado de Normal caso o tempo de mega chegue a 0 ou inferior. Por fim, temos a função __passTime__ que chama a função __executajogadas__ que permite atualizar o estado dos jogo
 depois de cada iteração.

= Discussão e conclusão
 Deste modo , o grupo ficou supreendido com o resultado depois desta tarefa, porque podemos visualisar o jogo do pacman a decorrer sucessivamente.
 Concluindo , o resultado foi o esperado pelo o grupo , já que conseguimos visualisar o jogo a decorrer , por outro lado , o grupo não conseguiu incorporar a Tarefa5 nesta tarefa.

 -}

module Tarefa4 where 

import Types
import Tarefa2
 
-- | __Tempo default utilizada para um step__
defaultDelayTime = 250 -- 250 ms 


-- | __Função principal__
passTime :: Int  -> State -> State
passTime x (State k l n) = executajogadas l x (State k l n)

-- | __Função que permite reduzir tempo quando está em Mega__
meginha :: Player -> Player 
meginha (Pacman(PacState (x,(a,b),z,t,h,l) q c d)) | d == Mega && q > 0 = (Pacman(PacState (x,(a,b),z,t,h,l) (q-0.25) c Mega)) -- Quando está em Mega e o tempo de Mega seja superior a 0, eles reduz 0.25 a esse tempo
												   | d == Mega && q <= 0  = (Pacman(PacState (x,(a,b),z,t,h,l) 0 c Normal)) -- Quanod o tempo já é inferior ou a igual a 0, o Pacman volta para o Estado Normal
												   | otherwise = (Pacman(PacState (x,(a,b),z,t,h,l) q c d)) --  Para os restantes casos, o Pacman retorna igual

-- | __Função que executa as jogadas de todos os jogadores__
executajogadas :: [Player] -> Int -> State -> State
executajogadas [] i (State x l n) = (State x l n) --  Caso que a lista de jogadores é vazia
executajogadas (Pacman(PacState (x,(a,b),z,t,h,l) q c d):t1) i (State x1 l1 n) = executajogadas t1 i (play (Move x t) (State x1 l1 n))	--  Caso o Pacman, irá executar jogadas para o resto dos jogadores tendo o State atualizado						    
executajogadas (Ghost(GhoState (x,(a,b),z,t,h,l) d):t1) i (State x1 l1 n)| d == Alive = executajogadas t1 i (play (Move x t) (State x1 l1 n)) --  Caso o Ghost e seja Alive ele executa jogadas para os restantes tendo o State atualizado
													                     | d == Dead = if even i  then executajogadas t1 i (play (Move x t) (State x1 l1 n)) else executajogadas t1 i (State x1 l1 n) --  Caso o ghost esteja Dead , este irá reduzir sua velocidade , ou seja, irá andar de step em step (utilizamos a paridade do step)














-- na passtime teremos de verificar a velocidade do state para puder jogar com isso



--frameseguinte ::  Manager -> State