module Game (
    Player(..),
    Board,
    GameState(..),
    emptyBoard,
    makeMove,
    checkWin,
    isDraw,
    switchPlayer
) where

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
checkWin player gameBoard = any (all (== Just player)) (rows ++ cols ++ diags)
  where
    rows = gameBoard
    cols = transpose gameBoard
    diags = [[gameBoard !! i !! i | i <- [0..2]], [gameBoard !! i !! (2 - i) | i <- [0..2]]]
    transpose = foldr (zipWith (:)) (repeat [])

isDraw :: Board -> Bool
isDraw currentBoard = all (/= Nothing) (concat currentBoard)

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X
