module Main (main) where

import Game (switchPlayer, Player(..), GameState(..), emptyBoard, makeMove, checkWin, isDraw)
import UI (printBoard)
import System.IO (hFlush, stdout)

displayInstructions :: IO ()
displayInstructions = do
    putStrLn "Welcome to Tic-Tac-Toe!"
    putStrLn "Instructions:"
    putStrLn "Choose a cell by entering a number from 1 to 9."
    putStrLn "The board layout is as follows:"
    putStrLn " 1 | 2 | 3 "
    putStrLn "-----------"
    putStrLn " 4 | 5 | 6 "
    putStrLn "-----------"
    putStrLn " 7 | 8 | 9 "
    putStrLn "Type 'exit' to quit the game."
    putStrLn ""

gameLoop :: GameState -> IO ()
gameLoop state = do
    printBoard (board state)
    putStrLn $ "Player " ++ show (currentPlayer state) ++ "'s turn:"
    putStr "Enter your move: "
    hFlush stdout

    command <- getLine
    if command == "exit"
        then putStrLn "Thank you for playing!"
        else case parseMove command of
            Just (row, col) -> 
                case makeMove row col (board state) (currentPlayer state) of
                    Just newBoard -> do
                        let newState = GameState newBoard (switchPlayer (currentPlayer state))
                        if checkWin (currentPlayer state) newBoard
                            then putStrLn ("Player " ++ show (currentPlayer state) ++ " wins!")
                            else if isDraw newBoard
                                then putStrLn "It's a draw!"
                                else gameLoop newState
                    Nothing -> do
                        putStrLn "Invalid move! That cell is already occupied. Try again."
                        gameLoop state
            Nothing -> do
                putStrLn "Invalid input! Please enter a number from 1 to 9 or type 'exit' to quit."
                gameLoop state

main :: IO ()
main = do
    displayInstructions
    gameLoop $ GameState emptyBoard X

parseMove :: String -> Maybe (Int, Int)
parseMove input =
    case reads input of
        [(num, "")] -> 
            if num >= 1 && num <= 9
            then Just ((num - 1) `div` 3 + 1, (num - 1) `mod` 3 + 1)
            else Nothing
        _ -> Nothing
