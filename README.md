# Todo List Manager

A simple command-line Todo List application built using Haskell. This application allows users to manage tasks, including adding, viewing, editing, marking as complete, and deleting tasks. The tasks are saved in a file for persistent storage.

## Features

- **Add Tasks**: Quickly add new tasks with a description, priority (High, Medium, Low), and due date.
- **View Tasks**: Display all tasks or filter to see only incomplete tasks.
- **Edit Tasks**: Modify the description of existing tasks.
- **Complete Tasks**: Mark tasks as completed.
- **Delete Tasks**: Remove tasks from the list.
- **Persistent Storage**: Tasks are saved to a file (`tasks.txt`), allowing them to persist between sessions.
- **Help Menu**: Easily access available commands and usage instructions.

## Installation

### Prerequisites

- **GHC**: The Glasgow Haskell Compiler
- **Stack**: A Haskell build tool

### Steps

1. **Clone the repository**:
    ```bash
    git clone git@github.com:Ace-Interview-Prep/atc-haskell-projects.git
    ```
		git checkout 1-todo-manager

2. **Build the project**:
    ```bash
    stack build
    ```

3. **Run the application**:
    ```bash
    stack exec todo-list-exe
    ```

## Usage

Once the application is running, you will be presented with the following options:

- `add`: Add a new task.
- `view`: View all tasks.
- `view-incomplete`: View only incomplete tasks.
- `complete`: Mark a task as complete.
- `edit`: Edit an existing task.
- `delete`: Delete a task.
- `exit`: Exit the application.
- `--help` or `-h`: Display the help menu.

### Example

To add a new task, type `add`, then follow the prompts to enter the task details. For instance:


## Contributing

Contributions are welcome! If you have suggestions for improvements or features, feel free to fork the repository and submit a pull request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.



