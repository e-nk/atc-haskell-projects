module Task (Task(..), newTask) where

data Task = Task { taskId :: Int
                 , description :: String
                 , isCompleted :: Bool
                 , priority :: String
                 , dueDate :: String
                 } deriving (Show, Eq, Read)  -- Added `Read` here
								 
newTask :: Int -> String -> String -> String -> Task
-- newTask idDesc desc priority dueDate = Task idDesc desc False priority dueDate
newTask idDesc desc taskPriority taskDueDate = Task idDesc desc False taskPriority taskDueDate