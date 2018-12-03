{-------------------------------------------------------------------------
-- Created by Marco Soto                                                --
-- Provides implementation of logic for connect 4 board.                --
--------------------------------------------------------------------------}

module Board where
import System.IO
import Data.List

-- These are 'static' behaving variables used for easy access
mkPlayer = (1, 'X')--, "Player 1"]
mkOpponent = (2, 'O')--, "Player 2"]

togglePlayer player = if player == 1 then 2 else 1 -- Switch player

{-------------------------------------------------------------------------
    The following grouped functions are defined and required by the
    assignment. They implement the basic structure for a connect 4 game.
--------------------------------------------------------------------------}

-- Makes a new connect 4 board
mkBoard :: Int -> Int -> [[Int]]
mkBoard m n = replicate m (replicate n 0)

-- Fills the desired board position
dropInSlot :: [[Int]] -> Int -> Int -> [[Int]]
dropInSlot [] _ _ = [[]]
dropInSlot bd i p = do
    if i < 0 || i >= numSlot bd || not (isSlotOpen bd i) then []
    else dropInEmptySpace bd p ((boardHeight bd)-1) i

-- Checks if board slot is open
isSlotOpen :: [[Int]] -> Int -> Bool
isSlotOpen [[]] _ = False
isSlotOpen bd i = if bd!!0!!i == 0 then True else False

-- Returns the number of slots (# of columns) in the board
numSlot :: [[Int]] -> Int
numSlot bd = length (bd!!0)

-- Determines if the selected slot has at least one empty spot
isFull :: [[Int]] -> Bool
isFull bd = isBoardFull bd 0

-- Determines if the player entered winning move
isWonBy :: [[Int]] -> Int -> Bool
isWonBy bd p = checkCardinals bd p || checkDiagonals bd p

-- Constructs string that will represent the board
boardToStr :: [[Int]] -> String
boardToStr board = "\n" ++ boardToStrHelper board 0 ++ "\n"

-- Converts the player number to a string character for board printing
playerToChar :: Int -> Char
playerToChar p
    | p == 1 = '1'
    | p == 2 = '2'
    | otherwise = '?' -- Empty space



{-------------------------------------------------------------------------
    The following grouped functions are required for searching the board
    for winning sequences (called by 'isWonBy' function above). 
    The searches are recursively implemented.
--------------------------------------------------------------------------}

{-- The following functions are for cardinal directions --}

-- Check cardinal directions for winning player row
checkCardinals :: [[Int]] -> Int -> Bool
checkCardinals board player = checkCols board player 0
                            || checkRows board player 0

-- -- Iterates through all columns for winning player
checkCols :: [[Int]] -> Int -> Int -> Bool
checkCols board player x
    | x >= numSlot board = False
    | checkCol board player (x, 0) 0 = True
    | otherwise = checkCols board player (x+1)

-- Check individual column for winning player
checkCol :: [[Int]] -> Int -> (Int, Int) -> Int -> Bool
checkCol board player square count
    | count >= 4 = True
    | snd square >= boardHeight board = False
    | player == board!!(snd square)!!(fst square) = checkCol board player (fst square, snd square+1) (count+1)
    | otherwise = checkCol board player (fst square, snd square+1) 0

-- Iterates through all rows for winning player
checkRows :: [[Int]] -> Int -> Int -> Bool
checkRows board player y
    | y >= boardHeight board = False
    | checkRow board player (0, y) 0 = True
    | otherwise = checkRows board player (y+1)

-- Check individual row for winning player
checkRow :: [[Int]] -> Int -> (Int, Int) -> Int -> Bool
checkRow board player square count
    | count >= 4 = True
    | fst square >= numSlot board = False
    | player == board!!(snd square)!!(fst square) = checkRow board player (fst square+1, snd square) (count+1)
    | otherwise = checkRow board player (fst square+1, snd square) 0

{-- The following functions are for diagonal directions --}

-- Check diagonals (slashes) for winning player row
checkDiagonals :: [[Int]] -> Int -> Bool
checkDiagonals board player =  forwslashUpper board player 0
                            || forwslashLower board player 0
                            || backslashUpper board player 0
                            || backslashLower board player 0

-- Checks the upper half of the forwardslash shape for player's winning sequence
forwslashUpper :: [[Int]] -> Int -> Int -> Bool
forwslashUpper board player y
    | y >= boardHeight board = False
    | checkForwslash board player (0,y) 0 = True
    | otherwise = forwslashUpper board player (y+1)

-- Checks the bottom half of the forwardslash shape for player's winning sequence
forwslashLower :: [[Int]] -> Int -> Int -> Bool
forwslashLower board player x
    | x >= numSlot board = False
    | checkForwslash board player (x, boardHeight board-1) 0 = True
    | otherwise = forwslashLower board player (x+1)

-- Check single diagonal with forward slash shape for winning player
checkForwslash :: [[Int]] -> Int -> (Int, Int) -> Int -> Bool
checkForwslash board player square count
    | count >= 4 = True
    | snd square < 0 || fst square >= numSlot board = False
    | player == board!!(snd square)!!(fst square) = checkForwslash board player (fst square+1, snd square-1) (count+1)
    | otherwise = checkForwslash board player (fst square+1, snd square-1) 0

-- Checks the upper half of the backslash shape for player's winning sequence
backslashUpper :: [[Int]] -> Int -> Int -> Bool
backslashUpper board player x
    | x >= numSlot board = False
    | checkBackslash board player (x,0) 0 = True
    | otherwise = backslashUpper board player (x+1)

-- Checks the bottom half of the backslash shape for player's winning sequence
backslashLower :: [[Int]] -> Int -> Int -> Bool
backslashLower board player y
    | y >= boardHeight board = False
    | checkBackslash board player (0,y) 0 = True
    | otherwise = backslashLower board player (y+1)

-- Check diagonal with back slash shape for winning player
checkBackslash :: [[Int]] -> Int -> (Int, Int) -> Int -> Bool
checkBackslash board player square count
    | count >= 4 = True
    | snd square >= boardHeight board || fst square >= numSlot board = False
    | player == board!!(snd square)!!(fst square) = checkBackslash board player (fst square+1, snd square+1) (count+1)
    | otherwise = checkBackslash board player (fst square+1, snd square+1) 0



{-------------------------------------------------------------------------
    The following grouped functions are used for general board info
    gathering and process handling.
--------------------------------------------------------------------------}

-- Searches for empty slot in column and drops in first open space; NOTE: Assumes column is not full
dropInEmptySpace :: [[Int]] -> Int -> Int -> Int -> [[Int]]
dropInEmptySpace board player row col
    | row < 0 = board
    | board!!row!!col == 0 = dropIn board player row col -- Empty slot
    | otherwise = dropInEmptySpace board player (row-1) col -- Occupied slot

-- Determines if the board has no empty spaces left
isBoardFull :: [[Int]] -> Int -> Bool
isBoardFull board col = do
    if col >= numSlot board then True
    else if isSlotOpen board col then False
    else isBoardFull board (col+1)

-- Returns height (# of rows) of the board
boardHeight :: [[Int]] -> Int
boardHeight board = length board

-- Rebuilds board array with player piece in desired location
dropIn :: [[Int]] -> Int -> Int -> Int -> [[Int]]
dropIn board player row col = (take row board ++ [take col (board!!row) ++ [player] ++ drop (col+1) (board!!row)] ++ drop (row+1) board)

-- Used to traverse list and construct board string representation
boardToStrHelper :: [[Int]] -> Int -> String
boardToStrHelper [[]] _ = ""
boardToStrHelper board row = do
    if row >= boardHeight board then ""
    else " " ++ (intersperse ' ' [playerToSymbol (board!!row!!i) | i <- [0..length (board!!row)-1]]) ++ " \n" ++ boardToStrHelper board (row+1)

-- Changes a symbol from number to char for board representation
playerToSymbol :: Int -> Char
playerToSymbol p
    | p == 1 = snd mkPlayer
    | p == 2 = snd mkOpponent
    | otherwise = '.' -- Empty space
