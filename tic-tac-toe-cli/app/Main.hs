{-# LANGUAGE DeriveFunctor #-}

import System.Random (randomRIO)
import Data.Maybe (isJust, isNothing)
import Data.List (transpose)

data Player = X | O deriving (Show, Eq)
type Board = [[Maybe Player]]
data GameState = GameState { board :: Board, currentPlayer :: Player } deriving (Show)
data Scores = Scores { xWins :: Int, oWins :: Int, draws :: Int } deriving (Show)

emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Nothing)

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
            return (availableMoves !! idx)

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

-- Play again prompt
askPlayAgain :: IO Bool
askPlayAgain = do
    putStrLn "Play again? (y/n):"
    answer <- getLine
    case answer of
        "y" -> return True
        "n" -> return False
        _   -> askPlayAgain  -- Ask again if invalid input

-- Game loop
gameLoop :: GameState -> Scores -> IO ()
gameLoop state scores = do
    putStrLn $ "Player " ++ show (currentPlayer state) ++ "'s turn:"
    printBoard (board state)
    if currentPlayer state == O  -- AI's turn
        then do
            (row, col) <- aiMove (board state) (currentPlayer state)
            putStrLn $ "AI chooses: " ++ show (row * 3 + col + 1)
            case makeMove row col (board state) (currentPlayer state) of
                Just newBoard -> handleMove newBoard (switchPlayer (currentPlayer state)) scores
                Nothing -> gameLoop state scores
        else do
            putStr "Enter your move (1-9): "
            command <- getLine
            handlePlayerMove command state scores

handlePlayerMove :: String -> GameState -> Scores -> IO ()
handlePlayerMove command state scores =
    case command of
        "exit" -> putStrLn "Thank you for playing!"
        "help" -> displayInstructions >> gameLoop state scores
        _ -> case parseMove command of
            Just pos -> 
                let (row, col) = positionToCoordinates pos
                in case makeMove row col (board state) (currentPlayer state) of
                    Just newBoard -> handleMove newBoard (switchPlayer (currentPlayer state)) scores
                    Nothing -> do
                        putStrLn "That spot is already taken!"
                        gameLoop state scores
            Nothing -> do
                putStrLn "Invalid input! Please enter a number between 1 and 9."
                gameLoop state scores

parseMove :: String -> Maybe Int
parseMove input =
    case reads input of
        [(n, "")] | n >= 1 && n <= 9 -> Just n
        _ -> Nothing

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X

-- Handle game over and scores
handleMove :: Board -> Player -> Scores -> IO ()
handleMove b player scores = do
    if checkWin (switchPlayer player) b
        then do
            putStrLn $ "Player " ++ show (switchPlayer player) ++ " wins!"
            let updatedScores = updateScores (switchPlayer player) scores
            printScores updatedScores
            playAgain <- askPlayAgain
            if playAgain
                then gameLoop (GameState emptyBoard X) updatedScores
                else putStrLn "Goodbye!"
        else if isDraw b
            then do
                putStrLn "It's a draw!"
                let updatedScores = updateScoresDraw scores
                printScores updatedScores
                playAgain <- askPlayAgain
                if playAgain
                    then gameLoop (GameState emptyBoard X) updatedScores
                    else putStrLn "Goodbye!"
            else gameLoop (GameState b player) scores

-- Update the scores after a win
updateScores :: Player -> Scores -> Scores
updateScores X scores = scores { xWins = xWins scores + 1 }
updateScores O scores = scores { oWins = oWins scores + 1 }

-- Update the scores after a draw
updateScoresDraw :: Scores -> Scores
updateScoresDraw scores = scores { draws = draws scores + 1 }

printScores :: Scores -> IO ()
printScores scores = do
    putStrLn "Current Scores:"
    putStrLn $ "X Wins: " ++ show (xWins scores)
    putStrLn $ "O Wins: " ++ show (oWins scores)
    putStrLn $ "Draws: " ++ show (draws scores)

main :: IO ()
main = do
    displayInstructions
    let initialScores = Scores 0 0 0
    gameLoop (GameState emptyBoard X) initialScores
