# Tic Tac Toe - Command Line Game

## Overview
This is a simple command-line Tic Tac Toe game written in Haskell. Players can compete against each other or play against an AI opponent. The game is played on a 3x3 grid, and players take turns marking a cell with their symbol (X or O). The first player to get three marks in a row (horizontally, vertically, or diagonally) wins. If all cells are filled without a winner, the game ends in a draw.

## Features
- **Player vs Player mode**: Two players can play against each other by taking turns.
- **Player vs AI mode**: Play against a basic AI opponent that randomly selects its moves.
- **Scorekeeping**: Keep track of the number of wins for each player.
- **Help Menu**: Players can view instructions during gameplay by typing `help`.
- **Restart Option**: After each game, players can choose to play again or exit.
  
## How to Play
1. The game starts by asking the player to choose the game mode:
   - **(1)** Player vs Player
   - **(2)** Player vs AI
2. The board layout is displayed as a 3x3 grid with cells numbered from 1 to 9.
   - Example:
     ```
     1 | 2 | 3
     ---------
     4 | 5 | 6
     ---------
     7 | 8 | 9
     ```
3. Players take turns to input a number corresponding to the cell where they want to place their mark.
   - Player X moves first, followed by Player O or the AI.
4. The game checks for a winner after each move. If a player gets three marks in a row, they win the game.
5. If the board is full and no player has won, the game ends in a draw.

## Commands
- **Move**: To make a move, input a number from 1 to 9 corresponding to the desired cell.
- **Help**: Type `help` during the game to display the help menu with instructions.
- **Exit**: Type `exit` to quit the game at any time.

## Installation

To run this Tic Tac Toe game, you need to have Haskell and Stack installed on your machine.

### Steps:
1. **Clone the repository**:
   ```bash
   git clone git@github.com:e-nk/atc-haskell-projects.git
	 ```
	 	git checkout 2-tic-tac-toe

2. **Navigate into the project directory**:

   		cd tic-tac-toe-cli

3. **Build the project**:

   		stack run


## Dependencies
The game requires the following dependencies:

- **base**: The Haskell base library.
- **random**: For generating random moves for the AI.
- **maybe**: To handle optional values and missing data.

These dependencies are managed by Stack and will be automatically installed when you build the project.

## Help Menu
During gameplay, you can access the help menu by typing `help`. The help menu provides detailed instructions on how to make moves, restart the game, and other game-related information.

- Example Gameplay:
     ```
		 $ stack exec tic-tac-toe-cli-exe
			Board layout:
			1 | 2 | 3
			---------
			4 | 5 | 6
			---------
			7 | 8 | 9

			Choose game mode: (1) Player vs Player (2) Player 	vs AI
			2

			Player X's turn:
			. . .
			---------
			. . .
			---------
			. . .
			Enter your move (1-9): 4

			AI chooses: 1
			O . .
			---------
			X . .
			---------
			. . .

			Player X's turn:
			...


     ```
## Contributing
Feel free to fork this project and submit pull requests to improve or add features to the game. Contributions are welcome!

## Acknowledgements

This `README.md` gives a clear overview of the game, installation instructions, gameplay features, commands, and an example of how the game runs in the terminal.

This project was made with ❤️ and ☕ by:

[Enock Kipkoech](https://github.com/e-nk)