module Game (
    Player(..),
    Board,
    GameState(..),
    emptyBoard,
    makeMove,
    checkWin,
    isDraw,
    switchPlayer,
    aiMove
) where

import Data.List (transpose)
import System.Random (randomRIO)

data Player = X | O deriving (Show, Eq)

type Board = [[Maybe Player]]

data GameState = GameState
    { board :: Board
    , currentPlayer :: Player
    }

emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Nothing)

makeMove :: Int -> Int -> Board -> Player -> Maybe Board
makeMove row col currentBoard player =
    if currentBoard !! (row - 1) !! (col - 1) == Nothing
    then Just (replace row col (Just player) currentBoard)
    else Nothing

replace :: Int -> Int -> Maybe Player -> Board -> Board
replace row col value currentBoard =
    case splitAt (row - 1) currentBoard of
        (beforeRow, targetRow:afterRow) ->
            let newRow = replaceAt (col - 1) value targetRow
            in beforeRow ++ newRow : afterRow
        _ -> currentBoard

replaceAt :: Int -> Maybe Player -> [Maybe Player] -> [Maybe Player]
replaceAt idx newValue xs =
    case splitAt idx xs of
        (before, _:after) -> before ++ newValue : after
        _ -> xs

checkWin :: Player -> Board -> Bool
checkWin player currentBoard =
    let rows = currentBoard
        cols = transpose currentBoard
        diags = [ [currentBoard !! i !! i | i <- [0..2]], 
                   [currentBoard !! i !! (2 - i) | i <- [0..2]] ]
    in any (all (== Just player)) (rows ++ cols ++ diags)

isDraw :: Board -> Bool
isDraw currentBoard = all (/= Nothing) (concat currentBoard)

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X

aiMove :: Board -> Player -> IO (Int, Int)
aiMove board _ = do
    let emptyCells = [(row, col) | row <- [1..3], col <- [1..3], board !! (row - 1) !! (col - 1) == Nothing]
    if null emptyCells
        then error "No available moves for AI."
        else do
            randomIndex <- randomRIO (0, length emptyCells - 1)
            return (fst (emptyCells !! randomIndex), snd (emptyCells !! randomIndex))
