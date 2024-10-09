module TaskManager (addTask, viewTasks, markComplete, deleteTask, editTask, filterByCompletion) where
import Data.List (intercalate)
import Task (Task(..))

-- Function to add a new task to the list
addTask :: String -> String -> String -> [Task] -> [Task]
addTask desc taskPriority taskDueDate tasks =
    let newId = if null tasks then 1 else (maximum (map taskId tasks) + 1)
    in Task newId desc False taskPriority taskDueDate : tasks

-- Function to view tasks as a formatted string
viewTasks :: [Task] -> String
viewTasks tasks = intercalate "\n" (map show tasks)  -- Converts each task to a string and joins with newlines

-- Function to mark a task as completed
markComplete :: [Task] -> Int -> [Task]
markComplete tasks taskIdValue = map (\t -> if taskId t == taskIdValue then t { isCompleted = True } else t) tasks

-- Function to delete a task by its ID
deleteTask :: Int -> [Task] -> [Task]
deleteTask _ [] = []
deleteTask targetTaskId (task@(Task tId _ _ _ _):tasks)
    | tId == targetTaskId = tasks  -- Skip this task to delete it
    | otherwise    = task : deleteTask targetTaskId tasks

-- Function to edit the description of a task
editTask :: Int -> String -> [Task] -> [Task]
editTask targetTaskId newDesc tasks = map updateTask tasks
  where
    updateTask task@(Task tId _ completed taskPriority taskDueDate) =
      if tId == targetTaskId then Task tId newDesc completed taskPriority taskDueDate else task

-- Function to filter tasks by completion status
filterByCompletion :: Bool -> [Task] -> [Task]
filterByCompletion completionStatus tasks = filter (\task -> isCompleted task == completionStatus) tasks
