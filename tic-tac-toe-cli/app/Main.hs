{-# LANGUAGE DeriveFunctor #-}

import System.Random (randomRIO)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.List (transpose)

data Player = X | O deriving (Show, Eq)
type Board = [[Maybe Player]]
data GameState = GameState { board :: Board, currentPlayer :: Player, gameMode :: GameMode } deriving (Show)
data GameMode = PvP | PvAI deriving (Show, Eq)

-- Scores data structure
data Scores = Scores { playerXScore :: Int, playerOScore :: Int } deriving (Show)

emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Nothing)

initialScores :: Scores
initialScores = Scores 0 0

-- Display instructions at the beginning or when help is requested
displayInstructions :: IO ()
displayInstructions = do
    putStrLn "\n=== Tic-Tac-Toe Help Menu ==="
    putStrLn "Instructions:"
    putStrLn "1. The game is played on a 3x3 grid."
    putStrLn "2. Players take turns placing their marks (X or O) in an empty cell."
    putStrLn "3. Enter a number from 1 to 9 to place your mark in the corresponding cell."
    putStrLn "   For example: Entering 1 places a mark in the top-left corner."
    putStrLn "4. The first player to get three of their marks in a row (horizontally, vertically, or diagonally) wins."
    putStrLn "5. Enter 'help' at any time to display this help menu."
    putStrLn "6. Enter 'exit' at any time to quit the game."
    putStrLn "7. After the game ends, you'll be asked whether to play again."
    putStrLn "Board layout:"
    putStrLn "1 | 2 | 3"
    putStrLn "---------"
    putStrLn "4 | 5 | 6"
    putStrLn "---------"
    putStrLn "7 | 8 | 9"
    putStrLn "=============================\n"

printBoard :: Board -> IO ()
printBoard b = do
    let displayRow r = unwords [showCell c | c <- r]
    putStrLn $ displayRow (b !! 0)
    putStrLn "---------"
    putStrLn $ displayRow (b !! 1)
    putStrLn "---------"
    putStrLn $ displayRow (b !! 2)
  where
    showCell Nothing  = "."
    showCell (Just X) = "X"
    showCell (Just O) = "O"

-- AI function
aiMove :: Board -> Player -> IO (Int, Int)
aiMove b player = do
    let availableMoves = [(r, c) | r <- [0..2], c <- [0..2], isNothing (b !! r !! c)]
    if null availableMoves
        then error "No available moves for AI."
        else do
            idx <- randomRIO (0, length availableMoves - 1)
            let (row, col) = availableMoves !! idx
            putStrLn $ "AI chooses: " ++ show (row * 3 + col + 1)  -- Display AI's move
            return (row, col)

-- Convert 1-9 input to row and column
positionToCoordinates :: Int -> (Int, Int)
positionToCoordinates pos = ((pos - 1) `div` 3, (pos - 1) `mod` 3)

-- Make a move on the board
makeMove :: Int -> Int -> Board -> Player -> Maybe Board
makeMove row col b player
    | isNothing (b !! row !! col) = Just (updateBoard row col b player)
    | otherwise = Nothing

updateBoard :: Int -> Int -> Board -> Player -> Board
updateBoard row col b player = take row b ++ [updatedRow] ++ drop (row + 1) b
  where
    updatedRow = take col (b !! row) ++ [Just player] ++ drop (col + 1) (b !! row)

checkWin :: Player -> Board -> Bool
checkWin player b = any (all (== Just player)) (rows ++ cols ++ diags)
  where
    rows = b
    cols = transpose b
    diags = [[b !! i !! i | i <- [0..2]], [b !! i !! (2 - i) | i <- [0..2]]]

isDraw :: Board -> Bool
isDraw b = all isJust (concat b)

-- Parse move input
parseMove :: String -> Maybe Int
parseMove input = case reads input of
    [(pos, "")] | pos >= 1 && pos <= 9 -> Just pos
    _                                   -> Nothing

-- Ask to play again
askPlayAgain :: IO Bool
askPlayAgain = do
    putStrLn "Play again? (y/n):"
    answer <- getLine
    case answer of
        "y" -> return True
        "n" -> return False
        _   -> askPlayAgain  -- Ask again if invalid input

-- Game mode selection
selectGameMode :: IO GameMode
selectGameMode = do
    putStrLn "Choose game mode: (1) Player vs Player (2) Player vs AI"
    choice <- getLine
    case choice of
        "1" -> return PvP
        "2" -> return PvAI
        _   -> do
            putStrLn "Invalid choice. Try again."
            selectGameMode

-- Update scores
updateScores :: Maybe Player -> Scores -> Scores
updateScores (Just X) (Scores x o) = Scores (x + 1) o
updateScores (Just O) (Scores x o) = Scores x (o + 1)
updateScores Nothing scores = scores  -- No change if it's a draw

displayScores :: Scores -> IO ()
displayScores (Scores x o) = do
    putStrLn $ "Scores:\nPlayer X: " ++ show x ++ "\nPlayer O: " ++ show o

gameLoop :: GameState -> Scores -> IO ()
gameLoop (GameState b player mode) scores = do
    printBoard b
    putStrLn $ "Player " ++ show player ++ "'s turn:"
    moveInput <- getLine
    case moveInput of
        "help" -> do
            displayInstructions
            gameLoop (GameState b player mode) scores
        "exit" -> putStrLn "Thanks for playing!"
        _ -> case parseMove moveInput of
            Just pos -> do
                let (row, col) = positionToCoordinates pos
                case makeMove row col b player of
                    Just newBoard -> do
                        if checkWin player newBoard
                            then do
                                printBoard newBoard
                                putStrLn $ "Player " ++ show player ++ " wins!"
                                let updatedScores = updateScores (Just player) scores
                                displayScores updatedScores
                                playAgain <- askPlayAgain
                                if playAgain
                                    then gameLoop (GameState emptyBoard X mode) updatedScores
                                    else putStrLn "Thanks for playing!"
                        else if isDraw newBoard
                            then do
                                printBoard newBoard
                                putStrLn "It's a draw!"
                                let updatedScores = updateScores Nothing scores
                                displayScores updatedScores
                                playAgain <- askPlayAgain
                                if playAgain
                                    then gameLoop (GameState emptyBoard X mode) updatedScores
                                    else putStrLn "Thanks for playing!"
                        else gameLoop (GameState newBoard (switchPlayer player) mode) scores
                    Nothing -> do
                        putStrLn "Invalid move, try again."
                        gameLoop (GameState b player mode) scores
            Nothing -> do
                putStrLn "Invalid input. Please enter a number from 1 to 9."
                gameLoop (GameState b player mode) scores

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X

main :: IO ()
main = do
    displayInstructions
    mode <- selectGameMode
    gameLoop (GameState emptyBoard X mode) initialScores
