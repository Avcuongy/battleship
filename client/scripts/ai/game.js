// AI Game Page
// Handles gameplay - shooting and game loop

const BOARD_SIZE = 10;
let myBoard = [];
let enemyBoard = [];
let myShips = [];
let myTurn = true;
let gameOver = false;
let playerName = '';
let enemyName = 'AI';

document.addEventListener('DOMContentLoaded', () => {
    // Check session and game state
    if (!SessionManager.isLoggedIn()) {
        window.location.href = '../initial.html';
        return;
    }
    
    const gameState = SessionManager.getGameState();
    if (!gameState || !gameState.roomId || !gameState.playerShips) {
        window.location.href = '../home.html';
        return;
    }
    
    // Load player data
    const session = SessionManager.getSession();
    playerName = session.player.name;
    myShips = gameState.playerShips;
    
    document.getElementById('player1Name').textContent = playerName;
    document.getElementById('player2Name').textContent = enemyName;
    
    // Initialize boards
    initializeBoards();
    renderMyBoard();
    renderEnemyBoard();
    
    // Start game
    updateTurnIndicator();
    
    console.log('Game started');
});

// Initialize empty boards
function initializeBoards() {
    // My board - shows my ships
    myBoard = createEmptyBoard();
    
    // Place my ships on my board
    myShips.forEach(ship => {
        ship.positions.forEach(([row, col]) => {
            myBoard[row][col] = { type: 'ship', hit: false };
        });
    });
    
    // Enemy board - empty, will be filled as we shoot
    enemyBoard = createEmptyBoard();
}

function createEmptyBoard() {
    const board = [];
    for (let row = 0; row < BOARD_SIZE; row++) {
        board[row] = [];
        for (let col = 0; col < BOARD_SIZE; col++) {
            board[row][col] = null;
        }
    }
    return board;
}

// Render my board (defensive board)
function renderMyBoard() {
    const boardElement = document.getElementById('playerBoard');
    boardElement.innerHTML = '';
    
    for (let row = 0; row < BOARD_SIZE; row++) {
        for (let col = 0; col < BOARD_SIZE; col++) {
            const cell = document.createElement('div');
            cell.className = 'cell';
            
            const cellData = myBoard[row][col];
            if (cellData) {
                if (cellData.type === 'ship') {
                    cell.classList.add('ship');
                    if (cellData.hit) {
                        cell.classList.add('hit');
                    }
                } else if (cellData.type === 'miss') {
                    cell.classList.add('miss');
                }
            }
            
            boardElement.appendChild(cell);
        }
    }
}

// Render enemy board (offensive board)
function renderEnemyBoard() {
    const boardElement = document.getElementById('enemyBoard');
    boardElement.innerHTML = '';
    
    for (let row = 0; row < BOARD_SIZE; row++) {
        for (let col = 0; col < BOARD_SIZE; col++) {
            const cell = document.createElement('div');
            cell.className = 'cell';
            cell.dataset.row = row;
            cell.dataset.col = col;
            
            const cellData = enemyBoard[row][col];
            if (cellData) {
                if (cellData.type === 'hit') {
                    cell.classList.add('hit');
                } else if (cellData.type === 'sunk') {
                    cell.classList.add('sunk');
                } else if (cellData.type === 'miss') {
                    cell.classList.add('miss');
                }
            }
            
            // Add click handler for shooting
            if (myTurn && !gameOver && !cellData) {
                cell.addEventListener('click', () => handleShoot(row, col));
                cell.style.cursor = 'pointer';
            }
            
            boardElement.appendChild(cell);
        }
    }
}

// Handle shooting at enemy board
async function handleShoot(row, col) {
    if (!myTurn || gameOver) return;
    
    // Disable further clicks
    myTurn = false;
    updateTurnIndicator();
    
    try {
        const gameState = SessionManager.getGameState();
        const session = SessionManager.getSession();
        
        console.log(`Shooting at [${row}, ${col}]`);
        const response = await BattleShipAPI.makeMove(
            gameState.roomId,
            session.player.id,
            [row, col]
        );
        
        console.log('Shoot response:', response);
        
        // Update enemy board
        if (response.result === 'Miss') {
            enemyBoard[row][col] = { type: 'miss' };
        } else if (response.result.startsWith('Hit')) {
            enemyBoard[row][col] = { type: 'hit' };
        } else if (response.result.startsWith('Sunk')) {
            enemyBoard[row][col] = { type: 'sunk' };
            // Mark all positions of sunk ship
            // (simplified - just mark the hit cell)
        }
        
        renderEnemyBoard();
        
        // Check if game over
        if (response.gameOver) {
            await handleGameOver(response.winner);
            return;
        }
        
        // AI's turn
        setTimeout(() => handleAITurn(), 1000);
        
    } catch (error) {
        console.error('Shoot failed:', error);
        alert('Không thể bắn. Vui lòng thử lại!');
        myTurn = true;
        updateTurnIndicator();
    }
}

// Handle AI's turn
async function handleAITurn() {
    console.log('AI turn...');
    
    try {
        // AI move is handled by server, we just need to get the result
        // In a real implementation, this would be via WebSocket
        // For now, simulate AI move
        
        await new Promise(resolve => setTimeout(resolve, 1000));
        
        // Generate random unshot position
        let row, col;
        do {
            row = Math.floor(Math.random() * BOARD_SIZE);
            col = Math.floor(Math.random() * BOARD_SIZE);
        } while (myBoard[row][col] && myBoard[row][col].type === 'miss');
        
        // Check if hit or miss
        const cellData = myBoard[row][col];
        if (cellData && cellData.type === 'ship') {
            // Hit!
            cellData.hit = true;
            console.log('AI hit at', [row, col]);
            
            // Check if all ships sunk
            const allSunk = checkAllShipsSunk();
            if (allSunk) {
                await handleGameOver('AI');
                return;
            }
        } else {
            // Miss
            myBoard[row][col] = { type: 'miss' };
            console.log('AI missed at', [row, col]);
        }
        
        renderMyBoard();
        
        // Back to player's turn
        myTurn = true;
        updateTurnIndicator();
        renderEnemyBoard(); // Re-render to enable clicks
        
    } catch (error) {
        console.error('AI turn failed:', error);
    }
}

// Check if all player ships are sunk
function checkAllShipsSunk() {
    for (let row = 0; row < BOARD_SIZE; row++) {
        for (let col = 0; col < BOARD_SIZE; col++) {
            const cell = myBoard[row][col];
            if (cell && cell.type === 'ship' && !cell.hit) {
                return false; // Found unhit ship cell
            }
        }
    }
    return true; // All ship cells are hit
}

// Update turn indicator
function updateTurnIndicator() {
    const indicator = document.getElementById('turnIndicator');
    const timer = document.getElementById('timer');
    
    if (myTurn) {
        indicator.textContent = 'Your Turn';
        indicator.className = 'turn-indicator your';
        document.getElementById('enemyBoardContainer').classList.add('active');
        document.getElementById('playerBoardContainer').classList.remove('active');
    } else {
        indicator.textContent = 'AI Turn';
        indicator.className = 'turn-indicator enemy';
        document.getElementById('enemyBoardContainer').classList.remove('active');
        document.getElementById('playerBoardContainer').classList.add('active');
    }
    
    // Reset timer (simplified)
    timer.textContent = '20s';
}

// Handle game over
async function handleGameOver(winner) {
    gameOver = true;
    
    console.log('Game over! Winner:', winner);
    
    try {
        const gameState = SessionManager.getGameState();
        const session = SessionManager.getSession();
        
        const winnerId = winner === playerName ? session.player.id : 'AI';
        const loserId = winner === playerName ? 'AI' : session.player.id;
        
        // Report game completion
        await BattleShipAPI.completeGame(
            gameState.roomId,
            winnerId,
            loserId
        );
        
        // Show game over message
        showGameOverMessage(winner);
        
    } catch (error) {
        console.error('Failed to complete game:', error);
        showGameOverMessage(winner);
    }
}

// Show game over message
function showGameOverMessage(winner) {
    // Create overlay
    const overlay = document.createElement('div');
    overlay.style.cssText = `
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(0, 0, 0, 0.8);
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        z-index: 1000;
    `;
    
    // Winner text
    const winnerText = document.createElement('div');
    winnerText.textContent = `${winner} CHIẾN THẮNG!`;
    winnerText.style.cssText = `
        font-size: 48px;
        font-weight: bold;
        color: #4caf50;
        margin-bottom: 30px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.5);
    `;
    
    // Return button
    const returnBtn = document.createElement('button');
    returnBtn.textContent = 'QUAY LẠI';
    returnBtn.style.cssText = `
        font-size: 24px;
        padding: 15px 40px;
        background: #2196f3;
        color: white;
        border: none;
        border-radius: 8px;
        cursor: pointer;
        font-weight: bold;
    `;
    
    returnBtn.addEventListener('click', () => {
        // Clear game state and return to home
        SessionManager.clearGameState();
        window.location.href = '../home.html';
    });
    
    overlay.appendChild(winnerText);
    overlay.appendChild(returnBtn);
    document.body.appendChild(overlay);
}
