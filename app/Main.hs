import TaskManager (addTask, viewTasks, markComplete, deleteTask, editTask, filterByCompletion)
import FileManager (saveTasks, loadTasks)
import Task (Task(..))
import Text.Read (readMaybe)

main :: IO ()
main = do
    tasks <- loadTasks "tasks.txt"
    mainLoop tasks

mainLoop :: [Task] -> IO ()
mainLoop tasks = do
    putStrLn "Options: [add, view, view-incomplete, complete, edit, delete, exit, --help, -h]"
    option <- getLine
    case option of
        "add" -> do
            putStrLn "Enter task description:"
            desc <- getLine
            putStrLn "Enter priority (High, Medium, Low):"
            taskPriority <- getLine
            putStrLn "Enter due date (YYYY-MM-DD):"
            taskDueDate <- getLine
            let newTasks = addTask desc taskPriority taskDueDate tasks
            saveTasks "tasks.txt" newTasks
            putStrLn "Task added!"
            mainLoop newTasks
        "view" -> do
            putStrLn (viewTasks tasks)
            mainLoop tasks
        "view-incomplete" -> do
            let incompleteTasks = filterByCompletion False tasks
            putStrLn (viewTasks incompleteTasks)
            mainLoop tasks
        "complete" -> do
            putStrLn "Enter task ID to mark as complete:"
            idStr <- getLine
            case readMaybe idStr of
                Just inputTaskId -> do
                    let updatedTasks = markComplete tasks inputTaskId
                    saveTasks "tasks.txt" updatedTasks
                    putStrLn "Task marked as complete!"
                    mainLoop updatedTasks
                Nothing -> do
                    putStrLn "Invalid task ID."
                    mainLoop tasks
        "edit" -> editTaskPrompt tasks
        "delete" -> deleteTaskPrompt tasks
        "--help" -> displayHelpMenu tasks  -- Pass tasks to displayHelpMenu
        "-h" -> displayHelpMenu tasks  -- Pass tasks to displayHelpMenu
        "exit" -> putStrLn "Goodbye!"
        _ -> do
            putStrLn "Invalid option."
            mainLoop tasks

-- Edit Task Prompt
editTaskPrompt :: [Task] -> IO ()
editTaskPrompt tasks = do
    putStrLn "Enter Task ID to edit:"
    idStr <- getLine
    case readMaybe idStr of
        Just inputTaskId -> do
            putStrLn "Enter new description:"
            newDesc <- getLine
            let updatedTasks = editTask inputTaskId newDesc tasks
            saveTasks "tasks.txt" updatedTasks
            putStrLn "Task edited!"
            mainLoop updatedTasks
        Nothing -> do
            putStrLn "Invalid task ID."
            mainLoop tasks

-- Delete Task Prompt
deleteTaskPrompt :: [Task] -> IO ()
deleteTaskPrompt tasks = do
    putStrLn "Enter Task ID to delete:"
    idStr <- getLine
    case readMaybe idStr of
        Just inputTaskId -> do
            let updatedTasks = deleteTask inputTaskId tasks
            saveTasks "tasks.txt" updatedTasks
            putStrLn "Task deleted!"
            mainLoop updatedTasks
        Nothing -> do
            putStrLn "Invalid task ID."
            mainLoop tasks

-- Function to display the help menu
displayHelpMenu :: [Task] -> IO ()
displayHelpMenu tasks = do
    putStrLn "Help Menu:"
    putStrLn "Available commands:"
    putStrLn "  add               - Add a new task."
    putStrLn "  view              - View all tasks."
    putStrLn "  view-incomplete   - View all incomplete tasks."
    putStrLn "  complete          - Mark a task as complete."
    putStrLn "  edit              - Edit an existing task."
    putStrLn "  delete            - Delete a task."
    putStrLn "  exit              - Exit the application."
    putStrLn "  --help, -h       - Show this help menu."
    mainLoop tasks  -- Call mainLoop with tasks
