module Types where

import Data.List

data State = State 
    {
        maze :: Maze
    ,   playersState :: [Player]
    ,   level :: Int
    }

type Maze = [Corridor]
type Corridor = [Piece]
data Piece =  Food FoodType | PacPlayer Player| Empty | Wall deriving (Eq)
data Player =  Pacman PacState | Ghost GhoState deriving (Eq)

data Orientation = L | R | U | D | Null deriving (Eq,Show)
data PacState= PacState 
    {   
        pacState :: PlayerState
    ,   timeMega :: Double
    ,   openClosed :: Mouth
    ,   pacmanMode :: PacMode
    
    } deriving Eq

data GhoState= GhoState 
    {
        ghostState :: PlayerState
    ,   ghostMode :: GhostMode
    } deriving Eq

type Coords = (Int,Int)
type PlayerState = (Int, Coords, Double , Orientation, Int, Int)
--                 (ID,  (x,y), velocity, orientation, points, lives) 
data Mouth = Open | Closed deriving (Eq,Show)
data PacMode = Dying | Mega | Normal deriving (Eq,Show)
data GhostMode = Dead  | Alive deriving (Eq,Show)
data FoodType = Big | Little deriving (Eq)
data Color = Blue | Green | Purple | Red | Yellow | None deriving Eq 

data Play = Move Int Orientation deriving (Eq,Show)

type Instructions = [Instruction]

data Instruction = Instruct [(Int, Piece)]
                 | Repeat Int deriving (Show, Eq)



instance Show State where
  show (State m ps p) = printMaze mz ++ "Level: " ++ show p ++ "\nPlayers: \n" ++ (foldr (++) "\n" (map (\y-> printPlayerStats y) ps))
                          where mz = placePlayersOnMap ps m

instance Show PacState where
   show ( PacState s o m Dying  ) =  "X"
   show ( PacState (a,b,c,R,i,l) _ Open m  ) =  "{"
   show ( PacState (a,b,c,R,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,L,i,l) _ Open m  ) =  "}"
   show ( PacState (a,b,c,L,i,l) _ Closed m  ) =  ">"
   show ( PacState (a,b,c,U,i,l) _ Open m  ) =  "V"
   show ( PacState (a,b,c,U,i,l) _ Closed m  ) =  "v"
   show ( PacState (a,b,c,D,i,l) _ Open m  ) =  "^"
   show ( PacState (a,b,c,D,i,l) _ Closed m  ) =  "|"
   show ( PacState (a,b,c,Null,i,l) _ Closed m  ) =  "<"
   show ( PacState (a,b,c,Null,i,l) _ Open m  ) =  "{"

instance Show Player where
   show (Pacman x ) =  show x
   show ( Ghost x ) =   show x

instance Show GhoState where
   show (GhoState x Dead ) =  "?"
   show (GhoState x Alive ) =  "M"

instance Show FoodType where
   show ( Big ) =  "o"
   show ( Little ) =  "."

instance Show Piece where
   show (  Wall ) =  "#" 
   show (  Empty ) =  " " 
   show (  Food z ) = (show z )  
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Normal ) ) ) = (show ( PacState (i, c, x, y,z,l) o m Normal)  ) 
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Mega   ) ) ) = (show ( PacState (i, c, x, y,z,l) o m Mega)  )
   show ( PacPlayer ( Pacman ( PacState (i, c, x, y,z,l) o m Dying   ) ) ) = (show ( PacState (i, c, x, y,z,l) o m Dying)  ) 
   show ( PacPlayer (Ghost z) ) = (show z)  


coloredString :: String -> Color -> String
coloredString x y
    | y == Blue ="\x1b[36m" ++  x ++ "\x1b[0m"
    | y == Red = "\x1b[31m" ++ x ++ "\x1b[0m"
    | y == Green = "\x1b[32m" ++ x ++ "\x1b[0m"
    | y == Purple ="\x1b[35m" ++ x ++ "\x1b[0m"
    | y == Yellow ="\x1b[33m" ++ x ++ "\x1b[0m"
    | otherwise =  "\x1b[0m" ++ x 


placePlayersOnMap :: [Player] -> Maze -> Maze
placePlayersOnMap [] x = x
placePlayersOnMap (x:xs) m = placePlayersOnMap xs ( replaceElemInMaze (getPlayerCoords x) (PacPlayer x) m )


printMaze :: Maze -> String
printMaze []  =  ""
printMaze (x:xs) = foldr (++) "" ( map (\y -> show y) x )  ++ "\n" ++ printMaze ( xs )

printPlayerStats :: Player -> String
printPlayerStats p = let (a,b,c,d,e,l) = getPlayerState p
                     in "ID:" ++ show a ++  " Points:" ++ show e ++ " Lives:" ++ show l ++"\n"

getPlayerID :: Player -> Int
getPlayerID (Pacman (PacState (x,y,z,t,h,l) q c d )) = x
getPlayerID  (Ghost (GhoState (x,y,z,t,h,l) q )) = x
 
getPlayerPoints :: Player -> Int
getPlayerPoints (Pacman (PacState (x,y,z,t,h,l) q c d )) = h
getPlayerPoints (Ghost (GhoState (x,y,z,t,h,l) q )) = h

setPlayerCoords :: Player -> Coords -> Player
setPlayerCoords (Pacman (PacState (x,y,z,t,h,l) q c d )) (a,b) = Pacman (PacState (x,(a,b),z,t,h,l) q c d )
setPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) q )) (a,b) = Ghost (GhoState (x,(a,b),z,t,h,l) q )


getPieceOrientation :: Piece -> Orientation
getPieceOrientation (PacPlayer p) =  getPlayerOrientation p
getPieceOrientation _ = Null

getPacmanMode :: Player -> PacMode
getPacmanMode (Pacman (PacState a b c d)) = d
  
getPlayerState :: Player -> PlayerState
getPlayerState (Pacman (PacState a b c d )) = a
getPlayerState (Ghost (GhoState a b )) = a

getPlayerOrientation :: Player -> Orientation
getPlayerOrientation (Pacman (PacState (x,y,z,t,h,l) q c d )) = t
getPlayerOrientation  (Ghost (GhoState (x,y,z,t,h,l) q )) = t

replaceElemInMaze :: Coords -> Piece -> Maze -> Maze
replaceElemInMaze (a,b) _ [] = []
replaceElemInMaze (a,b) p (x:xs) 
  | a == 0 = replaceNElem b p x : xs 
  | otherwise = x : replaceElemInMaze (a-1,b) p xs


replaceNElem :: Int -> a -> [a] -> [a]
replaceNElem i _ [] = [] 
replaceNElem i el (x:xs)
  |  i == 0 = el : xs 
  | otherwise =  x : replaceNElem (i-1) el xs

getPlayerCoords :: Player -> Coords
getPlayerCoords (Pacman (PacState (x,y,z,t,h,l) b c d )) = y
getPlayerCoords (Ghost (GhoState (x,y,z,t,h,l) b )) = y

-- | Função para me dar a vida do Jogador
getPlayervida :: Player -> Int
getPlayervida (Pacman (PacState (x,y,z,t,h,l) q c d )) = l
getPlayervida (Ghost (GhoState (x,y,z,t,h,l) q )) = l

-- | Função que encontra o id do Player
encontraid :: [Player] -> Int -> Player
encontraid [] _ = error "erro erro"     
encontraid (h:t) n | n == getPlayerID h = h
                   |otherwise = encontraid t n

semjoga :: [Player] -> Int -> [Player]
semjoga [] _ = [] 
semjoga (h:t) n | n == getPlayerID h = t
                |otherwise = h : semjoga t n


-- | Função que encontra os Ghosts
encontraghots :: [Player] -> [Player]
encontraghots [] = []
encontraghots ((Ghost(GhoState (x,(a,b),v,o,p,k) d1)):xs) = (Ghost(GhoState (x,(a,b),v,o,p,k) d1)) : encontraghots xs
encontraghots ((Pacman(PacState (x,(a,b),v,o,p,k) q1 c1 d1)):xs) = encontraghots xs

-- | Função que encontra o Pacman
encontrapacman :: [Player] -> Player
encontrapacman ((Pacman (PacState (x,(a,b),v,o,p,k) q1 c1 d1)):xs) = (Pacman (PacState (x,(a,b),v,o,p,k) q1 c1 d1))
encontrapacman ((Ghost(GhoState (x,(a,b),v,o,p,k) d1)):xs) = encontrapacman xs

pecaget :: Coords -> Maze -> Piece
pecaget (x,y) l = (l!!x)!!y

obterpeca :: Maze -> Coords -> Piece
obterpeca (h:t) (x,y) = (linhacheck (corredorcheck (h:t) x) y)

corredorcheck :: Maze -> Int -> Corridor
corredorcheck (h:t) x = posicao (h:t) x

linhacheck :: Corridor -> Int -> Piece
linhacheck (h:t) x = posicao (h:t) x 

posicao :: [a] -> Int -> a
posicao (h:t) 0 = h
posicao (h:t) x = posicao t (x-1) 

md :: Player -> Orientation -> Player
md (Pacman (PacState (x,(a,b),v,o,p,k) q1 c1 d1 )) new = (Pacman (PacState (x,(a,b),v,new,p,k) q1 c1 d1 ))
md (Ghost (GhoState (i,(a2,b2),v,o,p,k) d1)) new = (Ghost (GhoState (i,(a2,b2),v,new,p,k) d1))

mda :: [Player] -> Orientation -> Int -> [Player]
mda (h:t) o id 
                     | id == getPlayerID h = (md h o) : t
                     | otherwise = h : (mda t o id)

abreboca :: Player -> Player
abreboca (Pacman(PacState (x,(a,b),z,t,h,l) q c d)) | abrebocaaux (Pacman(PacState (x,(a,b),z,t,h,l) q c d)) == True = (Pacman(PacState (x,(a,b),z,t,h,l) q Closed d))
                          | abrebocaaux (Pacman(PacState (x,(a,b),z,t,h,l) q c d)) == False = (Pacman(PacState (x,(a,b),z,t,h,l) q Open d))

abrebocaaux :: Player -> Bool
abrebocaaux (Pacman(PacState (x,(a,b),z,t,h,l) q c d)) | c == Open = True
                                                       | otherwise = False
