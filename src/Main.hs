import Graphics.Gloss (play, white, red, green, Display(InWindow))
import Graphics.Gloss.Data.Picture
    (Picture, rectangleSolid, thickCircle, translate, pictures, color, rotate,
     blank)
import Graphics.Gloss.Interface.Pure.Game
    (Event (EventKey), Key (SpecialKey), KeyState (Up), SpecialKey (KeyF1, KeyF2, KeyF3, KeyF4, KeyF4, KeyF5, KeyF6, KeyF7))
import qualified Data.List 
import qualified Data.Maybe as M
import Data.Maybe
import Debug.Trace(trace)

-- CODE FROM US
main :: IO ()
main = play
    (InWindow "Connect4" (700, 600) (100, 100))
    white -- background color
    20
    emptyBoard -- initial world
    renderBoard -- function that makes the world a picture
    handleInput -- handle input events
    step -- step 

renderBoard :: Board -> Picture
renderBoard b =
    pictures $ [  --lineas a lo horizontal
                  translate 0 200 (rectangleSolid 700 3),
                  translate 0 100 (rectangleSolid 700 3),
                  translate 0 0 (rectangleSolid 700 3),
                  translate 0 (-100) (rectangleSolid 700 3),
                  translate 0 (-200) (rectangleSolid 700 3),
                  --lineas a lo vertical
                  translate 250 0 (rectangleSolid 3 600),
                  translate 150 0 (rectangleSolid 3 600),
                  translate 50 0 (rectangleSolid 3 600),
                  translate (-50) 0 (rectangleSolid 3 600),
                  translate (-150) 0 (rectangleSolid 3 600),
                  translate (-250) 0 (rectangleSolid 3 600)
               ] ++ -- aqui van las fichas
               [ translate (x*100) (y*100) $ 
                renderMarker (b??(relate y, relate x))
               | x <- [-3, -2, -1, 0, 1, 2, 3]
               , y <- [-2.5, -1.5, -0.5, 0.5, 1.5, 2.5] ]

renderMarker :: Tile -> Picture
renderMarker X = color red $ rotate 45 $
                     pictures [rectangleSolid 90 10, rectangleSolid 10 90]
renderMarker O = color green $ thickCircle 35 10
renderMarker _ = blank

relate :: Float -> Int
relate x = case x of
  (-3) -> 1
  (-2) -> 2
  (-1) -> 3
  0 -> 4
  1 -> 5
  2 -> 6
  3 -> 7
  (-2.5) -> 6
  (-1.5) -> 5
  (-0.5) -> 4
  (0.5) -> 3
  (1.5) -> 2
  (2.5) -> 1
  _ -> error "Bad number"

currentPlayer :: Board -> Tile
currentPlayer b | even $ length $ emptySpaces b = X
                | otherwise                   = O

handleInput :: Event -> Board -> Board
handleInput (EventKey (SpecialKey KeyF1) Up _ _) b =
  if b??y == EmptyTile && currentPlayer b == X && ((tileWins b X) == False) && (tileWins b O == False)
    then put b X y
    else b
    where y = (rowOfCol b (dimN dim, 1), 1)

handleInput (EventKey (SpecialKey KeyF2) Up _ _) b =
  if b??y == EmptyTile && currentPlayer b == X && ((tileWins b X) == False) && (tileWins b O == False)
    then put b X y
    else b
    where y = (rowOfCol b (dimN dim, 2), 2)

handleInput (EventKey (SpecialKey KeyF3) Up _ _) b =
  if b??y == EmptyTile && currentPlayer b == X && ((tileWins b X) == False) && (tileWins b O == False)
    then put b X y
    else b
    where y = (rowOfCol b (dimN dim, 3), 3)

handleInput (EventKey (SpecialKey KeyF4) Up _ _) b =
  if b??y == EmptyTile && currentPlayer b == X && ((tileWins b X) == False) && (tileWins b O == False)
    then put b X y
    else b
    where y = (rowOfCol b (dimN dim, 4), 4)

handleInput (EventKey (SpecialKey KeyF5) Up _ _) b =
  if b??y == EmptyTile && currentPlayer b == X && ((tileWins b X) == False) && (tileWins b O == False)
    then put b X y
    else b
    where y = (rowOfCol b (dimN dim, 5), 5)

handleInput (EventKey (SpecialKey KeyF6) Up _ _) b =
  if b??y == EmptyTile && currentPlayer b == X && ((tileWins b X) == False) && (tileWins b O == False)
    then put b X y
    else b
    where y = (rowOfCol b (dimN dim, 6), 6)

handleInput (EventKey (SpecialKey KeyF7) Up _ _) b =
  if b??y == EmptyTile && currentPlayer b == X && ((tileWins b X) == False) && (tileWins b O == False)
    then put b X y
    else b
    where y = (rowOfCol b (dimN dim, 7), 7)

handleInput _ b = b

step :: Float -> Board -> Board
step _ b =
    if currentPlayer b == O && (not . null . emptySpaces $ b) && ((tileWins b X) == False) && (tileWins b O == False)
    then put b O (computeMove O b) 
    else b

-- FUNCTIONS FROM GOMOKU
type Board  = [(Move, Tile)]
type Move   = (Int,Int)
data Tile = EmptyTile | X | O deriving (Eq, Show)
data Dimentions = Dim {dimN :: Int, dimM :: Int, dimK :: Int}

dim :: Dimentions
dim = Dim 6 7 4

emptyBoard :: Board
emptyBoard = [((x,y), EmptyTile) | x <- [1..(dimN dim)], y <- [1..(dimM dim)]]

(??) :: Board -> Move -> Tile
b??ij = M.fromMaybe EmptyTile (lookup ij b) 

put :: Board -> Tile -> Move -> Board
put b t move = case (putMaybe b t move) of 
  Just a -> a
  Nothing -> error $ show move

putMaybe :: Board -> Tile -> Move -> Maybe Board
putMaybe b t xy = case b??xy of
               EmptyTile -> Just $ map (\(ij,tij) -> if ij == xy then (ij,t) else (ij,tij)) b 
               _         -> Nothing

emptySpaces :: Board -> [(Int, Int)]
emptySpaces b = [(x, y) | x <- [1..6], y <- [1..7], b??(x,y) == EmptyTile]

validMoves :: Board -> [Move]
validMoves board  = [(row', col') | (row', col') <- [(rowOfCol board (dimN dim, col), col) | col <- [4,3,5,2,6,1,7] ], row' /= 0 ]

rowOfCol :: Board -> Move -> Int
rowOfCol board (row, col)
  | board ?? (row, col) == EmptyTile = row
  | otherwise = rowOfCol board (row-1, col)

showBoard :: Board -> String
showBoard b = let blist = boardAsList
              in  unlines [Data.List.intercalate "|" row | row <- blist]
              where
                boardAsList = [[show (b??(x,y)) | y <- [1..dimM dim]] | x <- [1..dimN dim]]

flipTile :: Tile -> Tile
flipTile X = O 
flipTile O = X 
flipTile _ = EmptyTile

tileWins :: Board -> Tile -> Bool
tileWins b t = 
   any (\col -> any (\row -> all (\k -> b??(row,col+k)   == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col)   == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col+k) == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row-k,col+k) == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] 

-- MINMAX STRATEGY
maxDepth :: Int
maxDepth = 2

winScore :: Int
winScore = 10000

debug :: Bool
debug = False

computeMove :: Tile -> Board -> Move
computeMove tile board 
  = snd $ maximum scoredMoves 
  where
        scoredMoves = zip scores moves
        scores      = [evaluateBoardMax (maxDepth-1) x tile $ put board tile x | x <- moves]
        moves       = validMoves board

evaluateBoardMax :: Int -> Move -> Tile -> Board -> Int
evaluateBoardMax depth lastMove tile board 
  | isJust score
  = if debug == True
      then trace ("win at move: " ++ show (maxDepth - depth)) $ fromJust score + depth
      else fromJust score + depth
  | depth == 0 || moves == []
  = if debug == True
      then trace ("lastMoveMH: "++show(lastMove)++" ,move# "++show(maxDepth-depth)++" ,score "++show(heuristic)++" "++replicate (depth *16) '*'++"\n"++showBoard board) $ heuristic 
      else heuristic
  | otherwise
  = if debug == True
      then trace ("lastMoveM: "++show(lastMove)++" ,move# "++show(maxDepth-depth)++" ,score "++show(auxMax (minBound::Int) depth tile board moves)++" "++replicate (depth *16) '*'++"\n"++showBoard board) $ auxMax (maxBound::Int) depth tile board moves
      else auxMax (maxBound::Int) depth tile board moves
  where
        heuristic   = (evaluateScore board tile) - (evaluateScore board (flipTile tile))
        score       = quickScoreBoard tile board lastMove
        moves       = validMoves board
        
auxMax :: Int -> Int -> Tile -> Board -> [Move] -> Int
auxMax minScore _ _ _ [] = minScore
auxMax minScore depth tile board (move:moves) 
  | minScore <= -(winScore + depth -3)
  = minScore
  | otherwise
  = auxMax (min minScore (evaluateBoardMin (depth-1) move (flipTile tile) $ put board (flipTile tile) move)) depth tile board moves

evaluateBoardMin :: Int -> Move -> Tile -> Board -> Int
evaluateBoardMin depth lastMove tile board
  | isJust score
  = if debug == True
      then trace ("loose at move: " ++ show (maxDepth - depth)) $ - (fromJust score + depth)
      else - (fromJust score + depth)
  | depth == 0 || moves == []
  = if debug == True
      then trace ("lastMoveOH: "++show(lastMove)++" ,move# "++show(maxDepth-depth)++" ,score "++show(heuristic)++" "++replicate (depth *16) '*'++"\n"++showBoard board) $ heuristic 
      else heuristic
  | otherwise
  = if debug == True
      then trace ("lastMoveO: "++show(lastMove)++" ,move# "++show(maxDepth-depth)++" ,score "++show(auxMin (minBound::Int) depth tile board moves)++" "++replicate (depth *16) '*'++"\n"++showBoard board) $ auxMin (minBound::Int) depth tile board moves
      else auxMin (minBound::Int) depth tile board moves
  where
        heuristic   = -(evaluateScore board tile) + (evaluateScore board (flipTile tile)) 
        score       = quickScoreBoard tile board lastMove
        moves       = validMoves board
        
auxMin :: Int -> Int -> Tile -> Board -> [Move] -> Int
auxMin maxScore _ _ _ [] = maxScore
auxMin maxScore depth tile board (move:moves) 
  | maxScore >= winScore + depth -3
  = maxScore
  | otherwise
  = auxMin (max maxScore (evaluateBoardMax (depth-1) move (flipTile tile) $ put board (flipTile tile) move)) depth tile board moves
   
------------------------------------------------End-game score--------------------------------------------------
----------------------------------------------------------------------------------------------------------------
quickTileWins :: Board -> Tile -> Move -> Bool
quickTileWins b t m = 
   any (\col -> any (\row -> all (\k -> b??(row,col+k)   == t) [0..dimK']) [fst m]) [(snd m - dimK')..(snd m)] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col)   == t) [0..dimK']) [(fst m - dimK')..(fst m)]) [snd m] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col+k) == t) [0..dimK']) [(fst m - dimK')..(fst m)]) [(snd m - dimK')..(snd m)] ||
   any (\col -> any (\row -> all (\k -> b??(row-k,col+k) == t) [0..dimK']) [(fst m + dimK'), (fst m + dimK')-1..(fst m)]) [(snd m - dimK')..(snd m)]
   where
     dimK' = (dimK dim)-1

quickScoreBoard :: Tile -> Board -> Move -> Maybe Int 
quickScoreBoard tile board move
  | quickTileWins board tile move
  = Just winScore
  | otherwise
  = Nothing

data Pos = Pos {getRow::Int, getCol:: Int}
    deriving (Show, Eq, Ord)

getSymbol :: Pos -> Board -> Maybe Tile
getSymbol position board 
--fromMaybe Nothing (lookup position b)
  | (x < 1) || (x > dimN dim) || (y < 1) || (y > dimM dim) = Nothing
  | otherwise = Just $ board??(x,y)
  where 
    x = getRow position
    y = getCol position

--Evaluates end of the n-tile combination (which has an empty tile on the other end) and returns a score key depending on it
evaluateEnd1 :: Int -> Tile -> Maybe Tile -> Int
evaluateEnd1 n tile end
  | isNothing end || fromJust end == (flipTile tile)
  = scoreRating n
  | otherwise
  = scoreRating (n+10)

--Evaluates end of the n-tile combination (which has an non-empty tile on the other end) and returns a score key depending on it
evaluateEnd2 :: Int -> Tile -> Maybe Tile -> Int
evaluateEnd2 n tile end
  | isNothing end || fromJust end == (flipTile tile)
  = 0
  | otherwise
  = scoreRating n

countRow :: Pos -> Tile -> Board -> Int -> Int
countRow position tile board number
  | getSymbol nextPosition board == Nothing
  = score
  | symbol == tile
  = countRow nextPosition tile board (number + 1)
  | otherwise
  = score + countRow nextPosition tile board 0
  where
    (Pos a b) = position
    nextPosition = Pos a (b+1)
    symbol = fromJust (getSymbol position board)
    score =
      if symbol == EmptyTile
        then evaluateEnd1 number tile $ getSymbol (Pos a (b-number-1)) board
        else if symbol == tile
          then evaluateEnd2 (number+1) tile $ getSymbol (Pos a (b-number)) board
          else evaluateEnd2 number tile $ getSymbol (Pos a (b-number-1)) board

countCol :: Pos -> Tile -> Board -> Int -> Int
countCol position tile board number
  | getSymbol nextPosition board == Nothing
  = score
  | symbol == tile
  = countCol nextPosition tile board (number + 1)
  | otherwise
  = score + countCol nextPosition tile board 0
  where
    (Pos a b) = position
    nextPosition= Pos (a+1) b
    symbol = fromJust (getSymbol position board)
    score =
      if symbol == EmptyTile
        then evaluateEnd1 number tile $ getSymbol (Pos (a-number-1) b) board
        else if symbol == tile
          then evaluateEnd2 (number+1) tile $ getSymbol (Pos (a-number) b) board --change
          else evaluateEnd2 number tile $ getSymbol (Pos (a-number-1) b) board

countDiagN :: Pos -> Tile -> Board -> Int -> Int
countDiagN position tile board number
  | getSymbol nextPosition board == Nothing
  = score
  | symbol == tile
  = countDiagN nextPosition tile board (number + 1)
  | otherwise
  = score + countDiagN nextPosition tile board 0
  where
    (Pos a b) = position
    nextPosition = Pos (a+1) (b+1)
    symbol = fromJust (getSymbol position board)
    score =
      if symbol == EmptyTile
        then evaluateEnd1 number tile $ getSymbol (Pos (a-number-1) (b-number-1)) board
        else if symbol == tile
          then evaluateEnd2 (number+1) tile $ getSymbol (Pos (a-number) (b-number)) board
          else evaluateEnd2 number tile $ getSymbol (Pos (a-number-1) (b-number-1)) board
          
countDiagP :: Pos -> Tile -> Board -> Int -> Int
countDiagP position tile board number
  | getSymbol nextPosition board == Nothing
  = score
  | symbol == tile
  = countDiagP nextPosition tile board (number + 1)
  | otherwise
  = score + countDiagP nextPosition tile board 0
  where
    (Pos a b) = position
    nextPosition = Pos (a-1) (b+1)
    symbol = fromJust (getSymbol position board)
    score =
      if symbol == EmptyTile
        then (evaluateEnd1 number tile $ getSymbol (Pos (a+number+1) (b-number-1)) board)
        else if symbol == tile
          then (evaluateEnd2 (number+1) tile $ getSymbol (Pos (a+number) (b-number)) board)
          else (evaluateEnd2 number tile $ getSymbol (Pos (a+number+1) (b-number-1)) board)

scoreRating :: Int -> Int
scoreRating number
  | number == 2 = 1
  | number == 3 = 10
  | number == 12 = 20
  | number == 13 = 100
  | otherwise = 0

evaluateScore :: Board -> Tile -> Int
evaluateScore board symbol
  | debug == True = trace ("^ scores: " ++ show ([scoreRow,scoreCol,scoreDiagN1 , scoreDiagN2 ,scoreDiagP1 , scoreDiagP2])++" -> [Row,Col,N1,N2,P1,P2]") $ scoreRow + scoreCol + scoreDiagN1 + scoreDiagN2 + scoreDiagP1 + scoreDiagP2
  | otherwise = scoreRow + scoreCol + scoreDiagN1 + scoreDiagN2 + scoreDiagP1 + scoreDiagP2
  where 
    scoreRow =  sum $ map (\x -> countRow (Pos x 1)  symbol board 0) [1..6]
    scoreCol = sum $ map (\y -> countCol (Pos 1 y) symbol board 0) [1..7]
    
    scoreDiagN1 =  sum $ map (\y -> countDiagN (Pos 1 y) symbol board 0) [2..4]
    scoreDiagN2 =  sum $ map (\x -> countDiagN (Pos x 1) symbol board 0) [1..3]

    scoreDiagP1 =  sum $ map (\y -> countDiagP (Pos 6 y) symbol board 0) [1..4]
    scoreDiagP2 =  sum $ map (\x -> countDiagP (Pos x 1) symbol board 0) [4..5]
