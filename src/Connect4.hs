module Main where 

import Types 
import Checks 
import Misc 
import Players 

import Player.BestNext    (playerBestNext )
import Player.Human       (playerHuman    )
import Player.Computer    (playerComputer )

import System.Exit
import System.Environment
import Control.Timeout
import Data.Time.Units (Second)
import Data.List  (lookup)


{- GUI

mport Graphics.Gloss (play, white, red, green, Display(InWindow))
import Graphics.Gloss.Data.Picture
    (Picture, rectangleSolid, thickCircle, translate, pictures, color, rotate,
     blank)
import Graphics.Gloss.Interface.Pure.Game
    (Event (EventKey), KeyState (Up), MouseButton (LeftButton),
     Key (MouseButton))
import qualified Data.Map as M
import Data.List (maximumBy)
import Data.Ord (comparing)

type Board = M.Map (Int, Int) Marker

data Marker = X | O | Blank deriving Eq

main :: IO ()
main = play
    (InWindow "Connect4" (300, 300) (100, 100))
    white
    20
    M.empty
    renderBoard
    handleInput
    step

renderMarker :: Marker -> Picture
renderMarker X = color red $ rotate 45 $
                     pictures [rectangleSolid 90 10, rectangleSolid 10 90]
renderMarker O = color green $ thickCircle 35 10
renderMarker _ = blank

getMarker :: Board -> Move -> Marker
getMarker = flip $ M.findWithDefault Blank

renderBoard :: Board -> Picture
renderBoard b =
    pictures $ [ translate    0   50  $ rectangleSolid 300   7
               , translate    0 (-50) $ rectangleSolid 300   6
               , translate   50    0  $ rectangleSolid   7 300
               , translate (-50)   0  $ rectangleSolid   6 300
               ] ++
               [ translate
                    ((fromIntegral x - 1) * 100)
                    ((fromIntegral y - 1) * 100) $
                    renderMarker $ getMarker b (x, y)
               | x <- [0..2]
               , y <- [0..2] ]

currentPlayer :: Board -> Marker
currentPlayer b | odd $ length $ availableMoves b = X
                | otherwise                       = O

-}

main :: IO ()
main = do
    (player1,player2) <-  getArgs >>= getPlayers
    putStrLn "Welcome to our connect4 game"
    rounds  <- prompt "How many rounds should we play?"
    score   <- playRounds (read rounds) player1  player2
    putStrLn $ showFinalScore score 


getPlayers :: [String] -> IO (Player, Player)
getPlayers input = 
  case input of 
    (n1:n2:_) -> (,) <$> lookupPlayer n1 <*> lookupPlayer n2 
    _         -> putStrLn "Give at least two player names" >> exitFailure

lookupPlayer :: String -> IO Player
lookupPlayer name =
  case lookup name players of 
    Just p -> return p 
    _      -> err 
  where
    err = putStrLn ("Player " ++ show name ++ "does not exists.") >> exitFailure

playRounds :: Int -> Player -> Player -> IO Score
playRounds rounds player1 player2 = 
  foldM (playRound pi1 pi2) [(pi1,0),(pi2,0)] [1..rounds]
  where 
    pi1 = PI player1 X 1 
    pi2 = PI player2 O 2 

playRound :: PlayerInfo -> PlayerInfo -> Score -> Int -> IO Score 
playRound p1 p2 score i = do 
   putStrLn ("Score:: " ++ showScore score)
   putStrLn ("Round " ++ show i ++ "!")
   putStrLn ((if (i `mod` 2 == 0) then show p2 else show p1)  ++ " plays first")
   result <- if (i `mod` 2 == 0) then play p2 p1 emptyBoard else play p1 p2 emptyBoard
   case result of 
      TimeOut p p' -> putStrLn (show p ++ " timed out after 30sec!\n\n") >> return (incr p' score)
      Invalid p p' -> putStrLn (show p ++ " made an invalid move!\n\n")  >> return (incr p' score)
      Wins p       -> putStrLn (show p ++ " wins!\n\n") >> return (incr p score)
      Tie          -> putStrLn "Its a tie!\n\n" >> return score 


play :: PlayerInfo -> PlayerInfo -> Board -> IO Result
play pi1@(PI p1 t1 _) pi2 board = do 
  timedMove <- timeout (30::Second) $ (playerMove p1) t1 board
  case timedMove of 
    Nothing   -> return $ TimeOut pi1 pi2 
    Just move -> 
      case putMaybe board t1 move of
        Nothing -> putStrLn "Invalid move." >> return (Invalid pi1 pi2)
        Just b  -> if tileWins b t1
                      then putStrLn (showWinningBoard b t1) >> return (Wins pi1)
                      else do putStrLn $ showBoardNew move b  
                              if checkFull b 
                                 then return Tie 
                                 else play pi2 pi1 b

                  
