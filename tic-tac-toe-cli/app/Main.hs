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

displayInstructions :: IO ()
displayInstructions = do
    putStrLn "Instructions:"
    putStrLn "1. The game is played on a 3x3 grid."
    putStrLn "2. Players take turns to place their marks (X or O) in an empty cell."
    putStrLn "3. The first player to get three of their marks in a row wins."
    putStrLn "4. Enter a number from 1 to 9 to place your mark in the corresponding cell."
    putStrLn "5. Type 'exit' to quit the game or 'help' to see this message again."
    putStrLn "Board layout:"
    putStrLn "1 | 2 | 3"
    putStrLn "---------"
    putStrLn "4 | 5 | 6"
    putStrLn "---------"
    putStrLn "7 | 8 | 9"

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
updateScores Nothing scores = scores

-- Display the scores
displayScores :: Scores -> IO ()
displayScores (Scores x o) = do
    putStrLn $ "Player X: " ++ show x
    putStrLn $ "Player O: " ++ show o

-- Main game loop
gameLoop :: GameState -> Scores -> IO ()
gameLoop state@(GameState b player mode) scores = do
    printBoard b
    if checkWin (switchPlayer player) b
        then do
            putStrLn $ "Player " ++ show (switchPlayer player) ++ " wins!"
            let updatedScores = updateScores (Just (switchPlayer player)) scores
            displayScores updatedScores
            playAgain <- askPlayAgain
            if playAgain
                then gameLoop (GameState emptyBoard X mode) updatedScores
                else putStrLn "Thanks for playing!"
        else if isDraw b
            then do
                putStrLn "It's a draw!"
                displayScores scores
                playAgain <- askPlayAgain
                if playAgain
                    then gameLoop (GameState emptyBoard X mode) scores
                    else putStrLn "Thanks for playing!"
            else do
                newBoard <- case mode of
                    PvP  -> playerMove player b
                    PvAI -> if player == X
                            then playerMove player b
                            else do
                                (aiRow, aiCol) <- aiMove b player
                                return (fromJust (makeMove aiRow aiCol b player))
                gameLoop (GameState newBoard (switchPlayer player) mode) scores

-- Player move input and validation
playerMove :: Player -> Board -> IO Board
playerMove player b = do
    putStrLn $ "Player " ++ show player ++ "'s turn:"
    input <- getLine
    case parseMove input of
        Just pos -> do
            let (row, col) = positionToCoordinates pos
            case makeMove row col b player of
                Just newBoard -> return newBoard
                Nothing -> do
                    putStrLn "That position is already taken. Try again."
                    playerMove player b
        Nothing -> do
            putStrLn "Invalid input. Please enter a number from 1 to 9."
            playerMove player b

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X

-- Main entry point
main :: IO ()
main = do
    displayInstructions
    mode <- selectGameMode
    gameLoop (GameState emptyBoard X mode) initialScores
