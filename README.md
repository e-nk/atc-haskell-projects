# Project 4: Basic Chat Server

## How to Run the Application

Install Obelisk if you haven't already. Follow the instructions at https://github.com/obsidiansystems/obelisk?tab=readme-ov-file#installing-obelisk.

Start the Obelisk full-stack server by navigating to the project folder and running in your terminal:

```bash
ob run
```

This will load your development environment with all the necessary Haskell dependencies and start an Obelisk server on `http://localhost:8000`. Navigate to this address in your browser to see the web application.

You can also run the program in an Obelisk REPL using [GHCi](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html):

```bash
ob repl
```

## Software Requirements

### Basic Functionality:
- Users can connect to the server via a command-line client.
- Users can send messages to a common chatroom where all connected users can see the messages.
- The server should broadcast incoming messages to all connected clients.
- Users should be able to disconnect from the server cleanly.

### Advanced Features (Optional):
- Implement private messaging between users.
- Allow users to join different chatrooms or create their own.
- Add basic user authentication for joining the chat (username/password).
- Provide command-line options for configuring the server (e.g., port number).

### User Interface:
- The client should provide a simple command-line interface for sending and receiving messages.
- Display a list of connected users and chatrooms (if implemented).

### Error Handling:
- Handle connection errors gracefully, both on the server and client sides.
- Ensure that user messages are correctly delivered or inform users if there was an issue.

### Code Structure:
- The code should be organized into modules separating concerns (e.g., networking, message handling, user management).
- Utilize concurrency for handling multiple clients simultaneously.

## Acceptance Criteria:
- **Core Features:**
  - Multiple users should be able to connect to the server and communicate in real-time.
  - Messages should be broadcasted correctly and received by all users.

- **Advanced Features (if implemented):**
  - Private messaging should work without interfering with public chat.
  - Chatrooms should be functional and isolated from one another.

- **User Interface:**
  - The command-line interface should be clear and responsive.
  - Connected users and chatrooms (if implemented) should be displayed correctly.

- **Error Handling:**
  - The server should handle user disconnections and connection errors smoothly.
  - Users should receive appropriate feedback if their message fails to send.

- **Code Quality:**
  - The code should be clean, modular, and well-documented.
  - Haskell best practices, especially concerning concurrency, should be followed.

## Rubric:

### Basic Functionality (40 points):
- Server and client setup (10 points)
- Message broadcasting (10 points)
- Multiple clients communication (10 points)
- Clean disconnection handling (10 points)

### Advanced Features (Optional - 20 points):
- Private messaging (10 points)
- Chatrooms (10 points)

### User Interface (20 points):
- Client UI clarity and responsiveness (10 points)
- User and chatroom display (10 points)

### Error Handling (10 points):
- Graceful handling of connection errors (5 points)
- Feedback on message delivery issues (5 points)

### Code Quality (30 points):
- Modular design (10 points)
- Code cleanliness and readability (10 points)
- Use of Haskell and concurrency best practices (10 points)

**Total: 120 points (100 if advanced features are not implemented)**
