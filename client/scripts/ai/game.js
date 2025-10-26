// AI Game Page
const TURN_TIME = 20;

let gameState = { 
  gameId: null, 
  playerId: null, 
  playerName: null, 
  playerFleet: [], 
  isPlayerTurn: true, 
  isGameOver: false 
};

let turnTimer = null;
let remainingTime = TURN_TIME;

function init() {
  console.log('AI Game - Init');
  
  const gameId = Storage.getGameId();
  const player = Storage.getPlayer();
  const fleet = Storage.getFleet();
  
  if (!gameId || !player.playerId || !fleet || fleet.length !== 5) {
    console.error('Missing data');
    alert('Thiếu dữ liệu game!');
    window.location.href = './setup.html';
    return;
  }
  
  gameState.gameId = gameId;
  gameState.playerId = player.playerId;
  gameState.playerName = player.playerName;
  gameState.playerFleet = fleet;
  
  console.log('Loaded:', { gameId, player: player.playerName, fleet: fleet.length });
  
  document.getElementById('player1Name').textContent = player.playerName;
  document.getElementById('player2Name').textContent = 'AI';
  
  const convertedFleet = convertFleet(fleet);
  if (convertedFleet.length === 0) {
    alert('Lỗi convert fleet!');
    window.location.href = './setup.html';
    return;
  }
  
  if (typeof AIEngine === 'undefined') {
    alert('AIEngine không load!');
    window.location.href = './setup.html';
    return;
  }
  
  AIEngine.initGame(convertedFleet);
  
  Board.render('playerBoard', true);
  Board.render('enemyBoard', false);
  
  fleet.forEach(ship => Board.placeShip('playerBoard', ship));
  
  // Enable enemy board clicks
  enableEnemyBoardClicks();
  
  // Setup modal buttons
  document.getElementById('playAgainBtn')?.addEventListener('click', () => {
    window.location.href = '../home.html';
  });
  document.getElementById('homeBtn')?.addEventListener('click', () => {
    window.location.href = '../home.html';
  });
  
  startTimer();
  updateFireIndicator(); // Set to default "FIRE"
  
  console.log('Game ready');
}

function convertFleet(fleet) {
  const sizes = { Carrier: 5, Battleship: 4, Cruiser: 3, Submarine: 3, Destroyer: 2 };
  return fleet.map(ship => {
    const size = sizes[ship.shipType];
    const positions = [];
    for (let i = 0; i < size; i++) {
      if (ship.shipOrientation === 'Horizontal') {
        positions.push({ posRow: ship.shipPosition.posRow, posCol: ship.shipPosition.posCol + i });
      } else {
        positions.push({ posRow: ship.shipPosition.posRow + i, posCol: ship.shipPosition.posCol });
      }
    }
    return { shipType: ship.shipType, size, positions };
  });
}

function startTimer() {
  stopTimer();
  remainingTime = TURN_TIME;
  updateTimer();
  turnTimer = setInterval(() => {
    remainingTime--;
    updateTimer();
    if (remainingTime <= 0) handleTimeout();
  }, 1000);
}

function stopTimer() {
  if (turnTimer) {
    clearInterval(turnTimer);
    turnTimer = null;
  }
}

function updateTimer() {
  const el = document.getElementById('timer');
  if (!el) return;
  el.textContent = remainingTime + 's';
  if (remainingTime <= 5) el.style.color = '#ff1744';
  else if (remainingTime <= 10) el.style.color = '#ff9800';
  else el.style.color = '#4caf50';
}

function handleTimeout() {
  console.log('Timer timeout - treat as MISS');
  stopTimer();
  
  if (gameState.isGameOver) return;
  
  // Switch to AI turn
  switchToAITurn();
}

// Enable clicks on enemy board
function enableEnemyBoardClicks() {
  const enemyBoard = document.getElementById('enemyBoard');
  if (!enemyBoard) return;
  
  enemyBoard.addEventListener('click', handleEnemyBoardClick);
}

// Handle click on enemy board
function handleEnemyBoardClick(e) {
  if (gameState.isGameOver || !gameState.isPlayerTurn) return;
  
  const cell = e.target.closest('.cell');
  if (!cell) return;
  
  const row = parseInt(cell.dataset.row);
  const col = parseInt(cell.dataset.col);
  
  if (isNaN(row) || isNaN(col)) return;
  
  // Process attack
  const result = AIEngine.processPlayerAttack({ posRow: row, posCol: col });
  console.log('Player attack:', result);
  
  // Update cell visual
  if (result.result === 'hit' || result.result === 'sunk') {
    cell.classList.add('hit');
    cell.style.backgroundColor = '#ff1744'; // Red
    
    if (result.result === 'sunk') {
      showNotification(`SUNK ${result.shipType}`); // SUNK + Tên Thuyền
    } else {
      showNotification('HIT'); // Just HIT
    }
    
    // Check if player won
    if (AIEngine.allShipsSunk(AIEngine.aiBoard)) {
      endGame(true); // Player wins
      return;
    }
    
    // Player continues (no timer reset, no turn switch)
    console.log('HIT - Player continues');
    
  } else if (result.result === 'miss') {
    cell.classList.add('miss');
    cell.style.backgroundColor = '#9e9e9e'; // Gray
    showNotification('MISS'); // Just MISS
    
    // Switch to AI turn
    switchToAITurn();
    
  } else if (result.result === 'already_attacked') {
    // User assumes they won't click already attacked cells
    // But just in case, do nothing
    console.log('Already attacked - ignored');
  }
}

// Switch to AI turn
function switchToAITurn() {
  console.log('Switch to AI turn');
  stopTimer();
  
  gameState.isPlayerTurn = false;
  // No indicator update needed - keep showing FIRE
  
  // AI attacks immediately
  setTimeout(() => {
    if (gameState.isGameOver) return;
    
    const aiResult = AIEngine.processAIAttack();
    console.log('AI attack:', aiResult);
    
    // Update player board visual
    const playerBoard = document.getElementById('playerBoard');
    const cell = playerBoard.querySelector(`[data-row="${aiResult.position.posRow}"][data-col="${aiResult.position.posCol}"]`);
    
    if (cell) {
      if (aiResult.result === 'hit' || aiResult.result === 'sunk') {
        cell.classList.add('hit');
        cell.style.backgroundColor = '#ff1744';
        
        if (aiResult.result === 'sunk') {
          showNotification(`AI SUNK your ${aiResult.shipType}!`);
        }
        
        // Check if AI won
        if (AIEngine.allShipsSunk(AIEngine.playerBoard)) {
          endGame(false); // AI wins
          return;
        }
      } else {
        cell.classList.add('miss');
        cell.style.backgroundColor = '#9e9e9e';
      }
    }
    
    // Switch back to player turn
    switchToPlayerTurn();
  }, 500); // 500ms delay for visual effect
}

// Switch to player turn
function switchToPlayerTurn() {
  console.log('Switch to player turn');
  
  gameState.isPlayerTurn = true;
  updateFireIndicator(); // Reset to "FIRE"
  startTimer(); // Reset timer for new turn
}

// Update fire indicator to default "FIRE"
function updateFireIndicator() {
  const indicator = document.getElementById('fireIndicator');
  if (!indicator) return;
  
  indicator.textContent = 'FIRE';
  indicator.style.color = '#2a5298';
}

// Show notification (MISS / HIT / SUNK + shipType)
function showNotification(message) {
  const indicator = document.getElementById('fireIndicator');
  if (!indicator) return;
  
  indicator.textContent = message;
  indicator.style.color = '#2a5298';
  
  setTimeout(() => {
    updateFireIndicator(); // Reset to "FIRE"
  }, 1500);
}

// End game
async function endGame(playerWon) {
  console.log('Game over - Player won:', playerWon);
  
  gameState.isGameOver = true;
  stopTimer();
  
  // Update stats (saves to server: data/players/)
  await Storage.updateStats(playerWon);
  
  // Show modal
  const modal = document.getElementById('gameOverModal');
  const title = document.getElementById('modalTitle');
  const message = document.getElementById('modalMessage');
  const playerNameEl = document.getElementById('modalPlayerName');
  
  if (modal && title && message) {
    if (playerWon) {
      title.textContent = 'VICTORY!';
      message.textContent = 'YOU WIN';
      playerNameEl.textContent = gameState.playerName;
      modal.classList.add('win');
    } else {
      title.textContent = 'DEFEAT';
      message.textContent = 'AI WINS';
      playerNameEl.textContent = 'AI';
      modal.classList.add('lose');
    }
    
    modal.style.display = 'flex';
  }
}

function handleTimeout() {
  console.log('Timer timeout - treat as MISS');
  stopTimer();
  
  if (gameState.isGameOver) return;
  
  // Switch to AI turn
  switchToAITurn();
}

document.addEventListener('DOMContentLoaded', init);

window.addEventListener('beforeunload', async () => {
  if (!gameState.isGameOver) {
    await Storage.updateStats(false);
  }
});
