/**
 * AI Game Page
 * Handle attack gameplay against AI
 * Pure client-side game logic with AIEngine
 */

// ============================================================================
// Constants
// ============================================================================

const TURN_TIME_LIMIT = 20; // 20 seconds per turn

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
  aiHits: 0,
  attackCount: 0
};

// Timer state
let turnTimer = null;
let remainingTime = TURN_TIME_LIMIT;

// ============================================================================
// Initialization
// ============================================================================

/**
 * Initialize game page
 * Load saved state from backend and setup boards
 */
function initGamePage() {
  console.log('=== Initializing AI game page ===');
  
  try {
    // Step 1: Load game data from localStorage
    if (!loadGameData()) {
      console.error('Failed to load game data');
      return;
    }
    
    // Step 2: Convert fleet format for AIEngine
    console.log('→ Converting player fleet...');
    const convertedFleet = convertFleetToAIFormat(gameState.playerFleet);
    if (convertedFleet.length === 0) {
      console.error('Fleet conversion failed');
      window.location.href = './setup.html';
      return;
    }
    console.log('✓ Fleet converted:', convertedFleet);
    
    // Step 3: Initialize AI engine
    console.log('→ Initializing AI Engine...');
    if (typeof AIEngine === 'undefined') {
      console.error('AIEngine not loaded');
      window.location.href = './setup.html';
      return;
    }
    AIEngine.initGame(convertedFleet);
    console.log('✓ AI Engine initialized');
    
    // Step 4: Display player info in UI
    displayPlayerInfo();
    
    // Step 5: Initialize and render boards
    initBoards();
    
    // Step 6: Start timer (continuous countdown)
    startTurnTimer();
    
    console.log('✓ AI game initialized successfully');
    console.log('Game State:', gameState);
    
  } catch (error) {
    console.error('Fatal error initializing game:', error);
    window.location.href = './setup.html';
  }
}

/**
 * Convert fleet from setup format to AIEngine format
 * Setup format: {shipType, shipPosition: {posRow, posCol}, shipOrientation, shipHits}
 * AIEngine format: {shipType, size, positions: [{posRow, posCol}, ...]}
 */
function convertFleetToAIFormat(fleet) {
  const SHIP_SIZES = {
    'Carrier': 5,
    'Battleship': 4,
    'Cruiser': 3,
    'Submarine': 3,
    'Destroyer': 2
  };
  
  if (!fleet || !Array.isArray(fleet)) {
    console.error('Invalid fleet format');
    return [];
  }
  
  return fleet.map(ship => {
    if (!ship.shipType || !ship.shipPosition || !ship.shipOrientation) {
      console.error('Invalid ship data:', ship);
      return null;
    }
    
    const size = SHIP_SIZES[ship.shipType];
    if (!size) {
      console.error('Unknown ship type:', ship.shipType);
      return null;
    }
    
    const positions = [];
    
    // Calculate all positions based on orientation
    for (let i = 0; i < size; i++) {
      if (ship.shipOrientation === 'Horizontal') {
        positions.push({
          posRow: ship.shipPosition.posRow,
          posCol: ship.shipPosition.posCol + i
        });
      } else {
        positions.push({
          posRow: ship.shipPosition.posRow + i,
          posCol: ship.shipPosition.posCol
        });
      }
    }
    
    return {
      shipType: ship.shipType,
      size: size,
      positions: positions
    };
  }).filter(ship => ship !== null);
}

/**
 * Load game data from storage
 */
function loadGameData() {
  console.log('Loading game data from localStorage...');
  
  // Get game ID
  gameState.gameId = GameStorage.getGameId();
  if (!gameState.gameId) {
    console.error('No game ID found');
    window.location.href = './setup.html';
    return false;
  }
  console.log('Game ID loaded:', gameState.gameId);

  // Get player data
  const player = GameStorage.getPlayer();
  if (!player.playerId || !player.playerName) {
    console.error('No player data found');
    window.location.href = '../home.html';
    return false;
  }
  gameState.playerId = player.playerId;
  gameState.playerName = player.playerName;
  console.log('Player data loaded:', player);

  // Get fleet
  const fleet = GameStorage.getFleet();
  if (!fleet || fleet.length !== 5) {
    console.error('Invalid fleet in storage:', fleet);
    window.location.href = './setup.html';
    return false;
  }
  gameState.playerFleet = fleet;
  console.log('Fleet loaded:', fleet);

  return true;
}

/**
 * Display player info in UI
 */
function displayPlayerInfo() {
  const player1Name = document.getElementById('player1Name');
  const player2Name = document.getElementById('player2Name');
  
  if (!player1Name || !player2Name) {
    console.error('Player name elements not found');
    return;
  }
  
  player1Name.textContent = gameState.playerName;
  player2Name.textContent = 'AI';
  
  console.log('Player info displayed on UI');
}

// ============================================================================
// Board Initialization
// ============================================================================

/**
 * Initialize both boards
 * Left board: Player's ships (visible)
 * Right board: Enemy/AI ships (hidden)
 */
function initBoards() {
  console.log('Initializing game boards...');
  
  // Validate board elements exist
  const playerBoardElement = document.getElementById('playerBoard');
  const enemyBoardElement = document.getElementById('enemyBoard');
  
  if (!playerBoardElement || !enemyBoardElement) {
    console.error('Board elements not found');
    return;
  }
  
  // Render player board (with ships visible)
  Board.render('playerBoard', true);
  console.log('✓ Player board rendered');
  
  // Display player's fleet on their board
  if (gameState.playerFleet && gameState.playerFleet.length === 5) {
    gameState.playerFleet.forEach((ship, index) => {
      try {
        Board.placeShip('playerBoard', ship);
        console.log(`Ship ${index + 1}/5 placed:`, ship);
      } catch (error) {
        console.error(`Error placing ship ${index + 1}:`, error);
      }
    });
    console.log('All player ships displayed');
  } else {
    console.error('Invalid fleet data');
  }
  
  // Render enemy board (AI ships hidden)
  Board.render('enemyBoard', false);
  console.log('Enemy board rendered (AI ships hidden)');
  
  console.log('Boards initialization complete');
}

// ============================================================================
// Timer Management (Continuous Countdown - NO RESET on HIT)
// ============================================================================

/**
 * Start turn timer - 20 second continuous countdown
 * Timer does NOT reset when player hits
 * Timer only resets on: MISS or TIME EXPIRED
 */
function startTurnTimer() {
  console.log('→ Starting turn timer (20s continuous countdown)');
  
  // Clear any existing timer
  stopTurnTimer();
  
  // Reset to 20 seconds
  remainingTime = TURN_TIME_LIMIT;
  updateTimerDisplay();
  
  // Start countdown (1 second interval)
  turnTimer = setInterval(() => {
    remainingTime--;
    updateTimerDisplay();
    
    // Time expired - end turn
    if (remainingTime <= 0) {
      console.log('⏰ Time expired!');
      handleTimeExpired();
    }
  }, 1000);
  
  console.log('✓ Timer started');
}

/**
 * Stop turn timer
 */
function stopTurnTimer() {
  if (turnTimer) {
    clearInterval(turnTimer);
    turnTimer = null;
    console.log('⏸ Timer stopped');
  }
}

/**
 * Update timer display in UI
 */
function updateTimerDisplay() {
  const timerEl = document.getElementById('timer');
  if (!timerEl) {
    console.error('Timer element not found');
    return;
  }
  
  timerEl.textContent = `${remainingTime}s`;
  
  // Color warning when time is low
  if (remainingTime <= 5) {
    timerEl.style.color = '#ff1744'; // Red
  } else if (remainingTime <= 10) {
    timerEl.style.color = '#ff9800'; // Orange
  } else {
    timerEl.style.color = '#4caf50'; // Green
  }
}

/**
 * Handle time expired - end player's turn
 */
function handleTimeExpired() {
  console.log('⏰ Time expired - ending turn');
  
  stopTurnTimer();
  
  // Disable attacks
  // Board.disableAttacks('enemyBoard');
  
  // Update turn indicator
  updateTurnIndicator(false);
  
  // TODO: AI counter-attack logic here
  // For now, just restart timer after delay
  setTimeout(() => {
    console.log('→ Restarting turn after timeout');
    updateTurnIndicator(true);
    startTurnTimer();
  }, 2000);
}

/**
 * Update turn indicator UI
 * @param {boolean} isPlayerTurn
 */
function updateTurnIndicator(isPlayerTurn) {
  const turnIndicator = document.getElementById('turnIndicator');
  
  if (!turnIndicator) {
    console.error('Turn indicator element not found');
    return;
  }
  
  if (isPlayerTurn) {
    turnIndicator.textContent = 'Your Turn';
    turnIndicator.className = 'turn-indicator your';
  } else {
    turnIndicator.textContent = "AI's Turn";
    turnIndicator.className = 'turn-indicator opponent';
  }
}

// ============================================================================
// Game Entry Point
// ============================================================================

console.log('=== AI Game Script Loaded ===');
console.log('GameStorage:', typeof GameStorage);
console.log('Board:', typeof Board);
console.log('AIEngine:', typeof AIEngine);

document.addEventListener('DOMContentLoaded', () => {
  console.log('=== DOMContentLoaded Event Fired ===');
  try {
    initGamePage();
  } catch (error) {
    console.error('Fatal error in initGamePage:', error);
    console.error('Stack trace:', error.stack);
    window.location.href = './setup.html';
  }
});

// Cleanup on page unload
window.addEventListener('beforeunload', () => {
  if (!gameState.isGameOver) {
    // Game was abandoned - count as loss
    GameStorage.updateStats(false);
  }
});
