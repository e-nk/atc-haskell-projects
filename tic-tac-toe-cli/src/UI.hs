module UI (
    printBoard
) where

import Game

printBoard :: Board -> IO ()
printBoard gameBoard = do
    putStrLn $ unlines $ map showRow gameBoard
    where
        showRow :: [Maybe Player] -> String
        showRow = unwords . map showCell

        showCell :: Maybe Player -> String
        showCell Nothing = "."
        showCell (Just player) = show player
