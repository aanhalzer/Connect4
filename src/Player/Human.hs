module Player.Human (playerHuman) where 

import Types 
import Misc 

playerHuman :: Player 
playerHuman = Player humanMove "Human"

humanMove :: Tile -> Board -> IO Move
humanMove _ board = do
      input <- getLine
      let col = read input :: Int
      return (rowOfCol board (dimN dim, col), col)



