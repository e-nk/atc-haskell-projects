# Tic Tac Toe - Command Line Game

## Overview
This is a simple command-line Tic Tac Toe game written in Haskell. Players can compete against each other or play against an AI opponent. The game is played on a 3x3 grid, and players take turns marking a cell with their symbol (X or O). The first player to get three marks in a row wins.

## Features
- 2 Player mode (Player vs Player)
- Player vs AI mode
- Option to choose X or O as your mark
- Keep track of scores: wins, losses, and draws
- Command-line instructions for help during gameplay
- Play again prompt at the end of each game

## How to Play
- The game is played on a 3x3 grid. Each cell is numbered from 1 to 9.
- Players take turns entering a number corresponding to the cell they want to mark.
- The first player to get three marks in a row (horizontally, vertically, or diagonally) wins.
- If all cells are filled and no player has won, the game is a draw.
- Enter 'exit' to quit the game, or 'help' to display the instructions again.

## Instructions
To start the game, simply run the Haskell executable:

\`\`\`bash
stack run
\`\`\`

### Example of a Tic Tac Toe board layout:

\`\`\`
1 | 2 | 3
---------
4 | 5 | 6
---------
7 | 8 | 9
\`\`\`

### Sample Gameplay:

\`\`\`
Welcome to Tic Tac Toe!
Player X's turn:
Enter your move (1-9): 1

X . .
. . .
. . .
\`\`\`

## Dependencies
- Haskell
- Stack (Haskell build tool)

## How to Install and Run
1. Clone the repository:
   \`\`\`bash
   git clone https://github.com/yourusername/tic-tac-toe-cli.git
   \`\`\`
2. Navigate to the project directory:
   \`\`\`bash
   cd tic-tac-toe-cli
   \`\`\`
3. Build the project:
   \`\`\`bash
   stack build
   \`\`\`
4. Run the game:
   \`\`\`bash
   stack run
   \`\`\`

## License
This project is licensed under the MIT License."

# Write the content to README.md
echo "$readme_content" > README.md

# Print confirmation
echo "README.md created successfully!"