/**
 * AI Game Page
 * Handle attack gameplay against AI
 * Calls: POST /api/ai/attack
 */

// ============================================================================
// State Management
// ============================================================================

let gameState = {
  gameId: null,
  playerId: null,
  playerName: null,
  playerFleet: [],
  isPlayerTurn: true,
  isGameOver: false,
  playerHits: 0,
  aiHits: 0
};

// ============================================================================
// Initialization
// ============================================================================

/**
 * Initialize game page
 */
function initGamePage() {
  console.log('Initializing AI game page...');
  
  // Load game data
  if (!loadGameData()) return;
  
  // Display player info
  displayPlayerInfo();
  
  // Initialize boards
  initBoards();
  
  // Start game (player's turn)
  startPlayerTurn();
  
  console.log('AI game initialized');
}

/**
 * Load game data from storage
 */
function loadGameData() {
  // Get game ID
  gameState.gameId = Storage.getGameId();
  if (!gameState.gameId) {
    console.error('No game ID found, redirecting to setup');
    window.location.href = './setup.html';
    return false;
  }

  // Get player data
  const player = Storage.getPlayer();
  if (!player.playerId || !player.playerName) {
    console.error('No player data found, redirecting to home');
    window.location.href = '../home.html';
    return false;
  }

  gameState.playerId = player.playerId;
  gameState.playerName = player.playerName;

  console.log('Game data loaded:', gameState);
  return true;
}

/**
 * Display player info in UI
 */
function displayPlayerInfo() {
  const player1Name = document.getElementById('player1Name');
  const player2Name = document.getElementById('player2Name');
  
  if (player1Name) {
    player1Name.textContent = gameState.playerName;
  }
  
  if (player2Name) {
    player2Name.textContent = 'AI';
  }
}

// ============================================================================
// Board Initialization
// ============================================================================

/**
 * Initialize both boards
 */
function initBoards() {
  // Render player board (shows ships - will be updated by attack results)
  Board.render('playerBoard', true);
  
  // Render enemy board (hides ships)
  Board.render('enemyBoard', false);
  
  console.log('Boards initialized');
}

// ============================================================================
// Turn Management
// ============================================================================

/**
 * Start player's turn
 */
function startPlayerTurn() {
  console.log("Player's turn started");
  
  gameState.isPlayerTurn = true;
  
  // Update UI
  updateTurnIndicator(true);
  
  // Enable attacks on enemy board
  Board.enableAttacks('enemyBoard', handlePlayerAttack);
}

/**
 * Start AI's turn (wait for response from attack API)
 */
function startAITurn() {
  console.log("AI's turn started");
  
  gameState.isPlayerTurn = false;
  
  // Update UI
  updateTurnIndicator(false);
  
  // Disable attacks
  Board.disableAttacks('enemyBoard');
  
  // AI attack is handled in the attack response (aarAiResult)
}

/**
 * Update turn indicator UI
 * @param {boolean} isPlayerTurn
 */
function updateTurnIndicator(isPlayerTurn) {
  const turnIndicator = document.getElementById('turnIndicator');
  
  if (turnIndicator) {
    if (isPlayerTurn) {
      turnIndicator.textContent = 'Your Turn';
      turnIndicator.className = 'turn-indicator your';
    } else {
      turnIndicator.textContent = "AI's Turn";
      turnIndicator.className = 'turn-indicator opponent';
    }
  }
}

// ============================================================================
// Attack Handling
// ============================================================================

/**
 * Handle player attack on enemy board
 * @param {{posRow: number, posCol: number}} position
 */
async function handlePlayerAttack(position) {
  console.log('Player attacking:', position);
  
  // Disable further attacks during processing
  Board.disableAttacks('enemyBoard');
  
  try {
    // Call API
    const response = await API.attackAI(gameState.gameId, position);
    
    if (!response) {
      throw new Error('No response from server');
    }

    console.log('Attack response:', response);
    
    // Process player's attack result
    processPlayerAttackResult(response.aarPlayerResult);
    
    // Check game over
    if (response.aarGameOver) {
      handleGameOver(response.aarWinner);
      return;
    }

    // Process AI's counter-attack
    if (response.aarAiResult) {
      setTimeout(() => {
        processAIAttackResult(response.aarAiResult);
        
        // Back to player's turn
        setTimeout(() => {
          startPlayerTurn();
        }, 1000);
      }, 1500);
    } else {
      // Player hit - continue player's turn
      startPlayerTurn();
    }
    
  } catch (error) {
    console.error('Attack error:', error);
    alert(`Lỗi tấn công: ${error.message}`);
    
    // Re-enable attacks
    startPlayerTurn();
  }
}

/**
 * Process player's attack result
 * @param {object} result - {arPosition, arResult: "hit"|"miss"|"sunk", arShipType?}
 */
function processPlayerAttackResult(result) {
  console.log('Player attack result:', result);
  
  const position = result.arPosition;
  
  if (result.arResult === 'hit' || result.arResult === 'sunk') {
    Board.markHit('enemyBoard', position);
    gameState.playerHits++;
    
    if (result.arResult === 'sunk') {
      showMessage(`You sunk AI's ${result.arShipType}!`, 'success');
    } else {
      showMessage('Hit!', 'success');
    }
  } else {
    Board.markMiss('enemyBoard', position);
    showMessage('Miss!', 'info');
  }
}

/**
 * Process AI's attack result
 * @param {object} result - {arPosition, arResult: "hit"|"miss"|"sunk", arShipType?}
 */
function processAIAttackResult(result) {
  console.log('AI attack result:', result);
  
  const position = result.arPosition;
  
  if (result.arResult === 'hit' || result.arResult === 'sunk') {
    Board.markHit('playerBoard', position);
    gameState.aiHits++;
    
    if (result.arResult === 'sunk') {
      showMessage(`AI sunk your ${result.arShipType}!`, 'danger');
    } else {
      showMessage('AI hit your ship!', 'warning');
    }
  } else {
    Board.markMiss('playerBoard', position);
    showMessage('AI missed!', 'info');
  }
}

// ============================================================================
// Game Over
// ============================================================================

/**
 * Handle game over
 * @param {string} winner - "player" or "ai"
 */
function handleGameOver(winner) {
  console.log('Game over! Winner:', winner);
  
  gameState.isGameOver = true;
  
  // Disable attacks
  Board.disableAttacks('enemyBoard');
  
  // Update stats
  const won = (winner === 'player');
  Storage.updateStats(won);
  
  // Show result
  const message = won 
    ? 'Bạn đã thắng !' 
    : 'AI đã thắng !';
  
  setTimeout(() => {
    if (confirm(`${message}\n\nPlay Again`)) {
      // Clear game ID and restart
      Storage.clearGameId();
      window.location.href = './setup.html';
    } else {
      // Return to home
      Storage.clearGameId();
      window.location.href = '../home.html';
    }
  }, 1000);
}

// ============================================================================
// UI Helpers
// ============================================================================

/**
 * Show temporary message
 * @param {string} message
 * @param {string} type - 'success', 'warning', 'danger', 'info'
 */
function showMessage(message, type = 'info') {
  console.log(`[${type.toUpperCase()}] ${message}`);
  
  // Optional: Show in UI (can create a message div)
  const messageDiv = document.getElementById('gameMessage');
  if (messageDiv) {
    messageDiv.textContent = message;
    messageDiv.className = `game-message ${type}`;
    messageDiv.style.display = 'block';
    
    setTimeout(() => {
      messageDiv.style.display = 'none';
    }, 2000);
  }
}

// ============================================================================
// Entry Point
// ============================================================================

document.addEventListener('DOMContentLoaded', initGamePage);

// Cleanup on page unload
window.addEventListener('beforeunload', () => {
  if (!gameState.isGameOver) {
    // Game was abandoned - count as loss
    Storage.updateStats(false);
  }
});
