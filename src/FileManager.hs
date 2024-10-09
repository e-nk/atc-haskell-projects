-- src/FileManager.hs
module FileManager (saveTasks, loadTasks) where

import Task (Task)             -- Ensure Task is imported if you need to use it here
-- import System.IO
-- import System.Directory (doesFileExist)
import Text.Read (readMaybe)

-- Save tasks to a file
saveTasks :: FilePath -> [Task] -> IO ()
saveTasks path tasks = writeFile path (show tasks)

-- Load tasks from a file
loadTasks :: FilePath -> IO [Task]
loadTasks path = do
    fileContent <- readFile path
    case readMaybe fileContent :: Maybe [Task] of  -- Specify the type to readMaybe
        Just tasks -> return tasks
        Nothing -> do
            putStrLn "Warning: Failed to parse tasks. Starting with an empty list."
            return []
