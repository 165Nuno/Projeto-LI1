module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4



data Manager = Manager 
    {   
        state   :: State   -- ^ Estado do jogo
    ,    pid    :: Int     -- ^ Id 
    ,    step   :: Int     -- ^ Numero que indica quantas iterações o jogo já passou
    ,    before :: Integer -- ^ Instante de tempo em que foi efetuada a ultima jogada
    ,    delta  :: Integer -- ^ Tempo decorrido em milisegundos desde a ultima jogada
    ,    delay  :: Integer -- ^ Representa o intervalo de tempo entre jogadas
    } 


loadManager :: Manager
loadManager = ( Manager (loadMaze "1.txt") 0 0 0 0 defaultDelayTime )

updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer k (Manager (State m l a) p t b d y)
                                            | k == KeyUpArrow = (Manager (State m z1 a) p t b d y)
                                            | k == KeyLeftArrow = (Manager (State m z1 a) p t b d y)
                                            | k == KeyRightArrow = (Manager (State m z1 a) p t b d y)
                                            | k == KeyDownArrow = (Manager (State m z1 a) p t b d y)
                                        where z1 = (mda l (keydi k) p)
                                             

keydi :: Key -> Orientation
keydi k | k == KeyUpArrow = U
        | k == KeyLeftArrow = L
        | k == KeyRightArrow = R
        | k == KeyDownArrow = D                                       

updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0 
                      drawString $ show (state man)
                    render
     
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

updateTime :: Integer -> Manager -> Manager
updateTime now man = man {delta = (now - before man)} 

resetTimer :: Integer -> Manager -> Manager
resetTimer now man = man {delta = 0, before = now} 

nextFrame :: Integer -> Manager -> Manager
nextFrame now man =  let update = (resetTimer now man) 
                     in update { state = (passTime (step man) (state man)), step = (step man) +1 }

loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorBlue ColorWhite  10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if ( delt > del )
    then loop w $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q')
                    then return ()
                    else loop w (updateTime now man)

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager

