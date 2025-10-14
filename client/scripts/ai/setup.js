// AI Setup Page
// Handles ship placement on the board

const BOARD_SIZE = 10;
const SHIP_TYPES = [
    { name: 'carrier', size: 5 },
    { name: 'battleship', size: 4 },
    { name: 'cruiser', size: 3 },
    { name: 'submarine', size: 3 },
    { name: 'destroyer', size: 2 }
];

let board = [];
let placedShips = [];
let currentShip = null;
let isHorizontal = true;

document.addEventListener('DOMContentLoaded', () => {
    // Check if user is logged in and has room
    if (!SessionManager.isLoggedIn()) {
        window.location.href = '../initial.html';
        return;
    }
    
    const gameState = SessionManager.getGameState();
    if (!gameState || !gameState.roomId) {
        window.location.href = '../home.html';
        return;
    }
    
    // Load player data
    const session = SessionManager.getSession();
    const player = session.player;
    
    document.getElementById('player1Name').textContent = player.name;
    
    // Initialize board
    initializeBoard();
    renderBoard();
    
    // Set up ship selection
    setupShipSelection();
    
    // Set up button handlers
    document.getElementById('resetBtn').addEventListener('click', resetBoard);
    document.getElementById('readyBtn').addEventListener('click', handleReady);
    
    // Set up right-click to rotate (prevent context menu)
    document.addEventListener('contextmenu', (e) => {
        e.preventDefault();
        if (currentShip) {
            isHorizontal = !isHorizontal;
            console.log('Ship orientation:', isHorizontal ? 'horizontal' : 'vertical');
        }
    });
    
    console.log('Setup page loaded');
});

// Initialize empty board
function initializeBoard() {
    board = [];
    for (let row = 0; row < BOARD_SIZE; row++) {
        board[row] = [];
        for (let col = 0; col < BOARD_SIZE; col++) {
            board[row][col] = null;
        }
    }
}

// Render board to DOM
function renderBoard() {
    const boardElement = document.getElementById('gameBoard');
    boardElement.innerHTML = '';
    
    for (let row = 0; row < BOARD_SIZE; row++) {
        for (let col = 0; col < BOARD_SIZE; col++) {
            const cell = document.createElement('div');
            cell.className = 'cell';
            cell.dataset.row = row;
            cell.dataset.col = col;
            
            // Apply ship styling if cell has ship
            if (board[row][col]) {
                cell.classList.add('ship');
            }
            
            // Add click handler for placement
            cell.addEventListener('click', () => handleCellClick(row, col));
            
            // Add hover effect for preview
            cell.addEventListener('mouseenter', () => handleCellHover(row, col));
            cell.addEventListener('mouseleave', () => clearPreview());
            
            boardElement.appendChild(cell);
        }
    }
}

// Setup ship selection
function setupShipSelection() {
    const shipItems = document.querySelectorAll('.ship-item');
    
    shipItems.forEach(item => {
        item.addEventListener('click', () => {
            if (item.classList.contains('placed')) {
                return; // Cannot select already placed ship
            }
            
            // Deselect all ships
            shipItems.forEach(s => s.classList.remove('selected'));
            
            // Select this ship
            item.classList.add('selected');
            
            currentShip = {
                name: item.dataset.name,
                size: parseInt(item.dataset.size),
                element: item
            };
            
            console.log('Selected ship:', currentShip.name);
        });
    });
}

// Handle cell click for ship placement
function handleCellClick(row, col) {
    if (!currentShip) {
        alert('Vui lòng chọn một tàu để đặt!');
        return;
    }
    
    // Check if can place ship
    if (!canPlaceShip(row, col, currentShip.size, isHorizontal)) {
        alert('Không thể đặt tàu tại vị trí này!');
        return;
    }
    
    // Place ship
    placeShip(row, col, currentShip.size, isHorizontal, currentShip.name);
    
    // Mark ship as placed
    currentShip.element.classList.add('placed');
    currentShip.element.classList.remove('selected');
    
    // Add ship to placed list
    const shipData = {
        type: currentShip.name,
        positions: [],
        isHorizontal: isHorizontal
    };
    
    for (let i = 0; i < currentShip.size; i++) {
        const r = isHorizontal ? row : row + i;
        const c = isHorizontal ? col + i : col;
        shipData.positions.push([r, c]);
    }
    
    placedShips.push(shipData);
    
    // Clear current ship
    currentShip = null;
    
    // Re-render board
    renderBoard();
    
    // Check if all ships placed
    if (placedShips.length === SHIP_TYPES.length) {
        document.getElementById('readyBtn').disabled = false;
        console.log('All ships placed!');
    }
}

// Check if can place ship at position
function canPlaceShip(row, col, size, horizontal) {
    for (let i = 0; i < size; i++) {
        const r = horizontal ? row : row + i;
        const c = horizontal ? col + i : col;
        
        // Check bounds
        if (r >= BOARD_SIZE || c >= BOARD_SIZE) {
            return false;
        }
        
        // Check if cell is occupied
        if (board[r][c]) {
            return false;
        }
    }
    
    return true;
}

// Place ship on board
function placeShip(row, col, size, horizontal, shipName) {
    for (let i = 0; i < size; i++) {
        const r = horizontal ? row : row + i;
        const c = horizontal ? col + i : col;
        board[r][c] = shipName;
    }
}

// Handle cell hover for preview
function handleCellHover(row, col) {
    if (!currentShip) return;
    
    clearPreview();
    
    // Show preview if can place
    if (canPlaceShip(row, col, currentShip.size, isHorizontal)) {
        for (let i = 0; i < currentShip.size; i++) {
            const r = isHorizontal ? row : row + i;
            const c = isHorizontal ? col + i : col;
            const cell = document.querySelector(`[data-row="${r}"][data-col="${c}"]`);
            if (cell) {
                cell.classList.add('preview');
            }
        }
    }
}

// Clear preview
function clearPreview() {
    document.querySelectorAll('.preview').forEach(cell => {
        cell.classList.remove('preview');
    });
}

// Reset board
function resetBoard() {
    if (!confirm('Bạn có chắc muốn đặt lại tất cả các tàu?')) {
        return;
    }
    
    initializeBoard();
    placedShips = [];
    currentShip = null;
    
    // Reset ship selection
    document.querySelectorAll('.ship-item').forEach(item => {
        item.classList.remove('placed', 'selected');
    });
    
    // Disable ready button
    document.getElementById('readyBtn').disabled = true;
    
    renderBoard();
    console.log('Board reset');
}

// Handle ready button click
async function handleReady() {
    const readyBtn = document.getElementById('readyBtn');
    readyBtn.disabled = true;
    readyBtn.textContent = 'ĐANG XỬ LÝ...';
    
    try {
        const gameState = SessionManager.getGameState();
        const session = SessionManager.getSession();
        
        // Send ship placement to server
        console.log('Sending ship placement:', placedShips);
        const response = await BattleShipAPI.setupShips(
            gameState.roomId,
            session.player.id,
            placedShips
        );
        
        console.log('Setup response:', response);
        
        // Save ships to game state
        SessionManager.updateGameState({
            playerShips: placedShips
        });
        
        // Navigate to game page
        readyBtn.textContent = 'THÀNH CÔNG!';
        setTimeout(() => {
            window.location.href = './game.html';
        }, 500);
        
    } catch (error) {
        console.error('Failed to setup ships:', error);
        alert('Không thể đặt tàu. Vui lòng thử lại!');
        readyBtn.disabled = false;
        readyBtn.textContent = 'SẴN SÀNG';
    }
}
