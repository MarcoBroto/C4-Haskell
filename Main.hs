{-------------------------------------------------------------------------
-- Created by Marco Soto                                                --
-- Program for playing connect 4 game against human opponent.           --
-- Written in Haskell; requires Board.hs file.                          --
--------------------------------------------------------------------------}

module Main where
import System.IO
import Board

main = do
    putStr "Welcome to Connect 4!\n"
    connect4 (mkBoard 6 7) 1 -- Start game with first player

-- Begins connect four game and continues recursively until win is detected or draw flag is set
connect4 :: [[Int]] -> Int -> IO()
connect4 [[]] _ = putStrLn "Invalid Input, exiting"
connect4 board player = do
    let prevPlayer = togglePlayer player -- Previous player who moved
    if isWonBy board prevPlayer then do
        putStr (boardToStr board)
        putStrLn ("Player " ++ [playerToChar prevPlayer] ++ " wins!!!")
    else if isFull board then putStrLn "\nIt's a draw."
    else do
        putStr (boardToStr board)
        putStrLn ("Player " ++ [playerToChar player] ++", choose a column to move:")
        temp <- getLine
        if temp == "" then do
            putStrLn "\n--Invalid Input, please enter a valid slot.--"
            connect4 board player
        else if temp == "-1" then putStrLn "\nGoodbye"
        else do
            let slot = (read temp)-1 -- Adjusted zero-indexed slot
            if slot < 0 || slot >= numSlot board then do
                putStrLn "\n--Invalid Input, please enter a valid slot.--"
                connect4 board player
            else if not (isSlotOpen board slot) then do
                putStrLn "\n--Slot is full! Please enter a valid slot.--"
                connect4 board player
            else connect4 (dropInSlot board slot player) (togglePlayer player)