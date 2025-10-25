/**
 * AI Setup Page
 * Place ships and start AI game
 * Calls: POST /api/ai/start
 */

// ============================================================================
// State Management
// ============================================================================

let playerData = null;
let isReady = false;

// ============================================================================
// Player Info Display
// ============================================================================

/**
 * Load and display player info
 */
function displayPlayerInfo() {
  playerData = Storage.getPlayer();
  
  if (!playerData.playerId || !playerData.playerName) {
    console.warn('No player data found, redirecting to home');
    window.location.href = '../home.html';
    return false;
  }

  // Display player name
  const player1Name = document.getElementById('player1Name');
  if (player1Name) {
    player1Name.textContent = playerData.playerName;
  }

  console.log('Player info displayed:', playerData);
  return true;
}

// ============================================================================
// Ship Placement
// ============================================================================

/**
 * Initialize ship placement
 */
function initShipPlacement() {
  // Render board
  Board.render('gameBoard', true);
  
  // Initialize drag-and-drop
  Ships.init('gameBoard', 'shipsSection');
  
  console.log('Ship placement initialized');
}

// ============================================================================
// Button Handlers
// ============================================================================

/**
 * Handle reset button click
 */
function handleReset() {
  console.log('Resetting ships...');
  
  Ships.reset();
  
  // Reset ready status
  isReady = false;
  updateReadyButton(false);
}

/**
 * Handle ready button click
 * Validate fleet and start AI game
 */
async function handleReady() {
  console.log('Ready button clicked');
  
  const fleet = Ships.getFleet();
  console.log('Current fleet:', fleet);
  console.log('Fleet size:', fleet.length);
  
  // FULL VALIDATION: Check if we have exactly 5 ships of correct types
  const validation = Validation.validateFleet(fleet);
  if (!validation.valid) {
    console.error('Fleet validation failed:', validation.errors);
    alert(`Lỗi đặt tàu:\n${validation.errors.join('\n')}`);
    return;
  }

  console.log('Fleet validation passed!');
  console.log('Fleet validated:', fleet);
  
  // Disable button during API call
  const readyBtn = document.getElementById('readyBtn');
  readyBtn.disabled = true;
  readyBtn.textContent = 'Đang khởi tạo...';

  try {
    // Call API to start AI game
    const response = await API.startAIGame(
      playerData.playerId,
      playerData.playerName,
      fleet
    );

    if (!response || response.asrStatus !== 'success') {
      throw new Error(response?.asrMessage || 'Failed to start AI game');
    }

    console.log('AI game started:', response);
    
    // Save game ID to storage
    Storage.saveGameId(response.asrGameId);
    
    // Navigate to game page
    window.location.href = './game.html';
    
  } catch (error) {
    console.error('Error starting AI game:', error);
    alert(`Không thể khởi tạo game: ${error.message}`);
    
    // Re-enable button
    readyBtn.disabled = false;
    readyBtn.textContent = 'SẴN SÀNG';
  }
}

/**
 * Update ready button state
 * @param {boolean} enabled
 */
function updateReadyButton(enabled) {
  const readyBtn = document.getElementById('readyBtn');
  if (readyBtn) {
    readyBtn.disabled = !enabled;
  }
}

// ============================================================================
// Event Listeners
// ============================================================================

/**
 * Setup event listeners
 */
function setupEventListeners() {
  const resetBtn = document.getElementById('resetBtn');
  const readyBtn = document.getElementById('readyBtn');
  
  if (resetBtn) {
    resetBtn.addEventListener('click', handleReset);
  }
  
  if (readyBtn) {
    readyBtn.addEventListener('click', handleReady);
  }
}

// ============================================================================
// Initialization
// ============================================================================

/**
 * Initialize setup page
 */
function initSetupPage() {
  console.log('Initializing AI setup page...');
  
  // Check game mode
  const gameMode = sessionStorage.getItem('battleship-game-mode');
  if (gameMode !== 'ai') {
    console.warn('Invalid game mode, redirecting to home');
    window.location.href = '../home.html';
    return;
  }

  // Display player info
  const hasPlayer = displayPlayerInfo();
  if (!hasPlayer) return;
  
  // Initialize ship placement
  initShipPlacement();
  
  // Setup event listeners
  setupEventListeners();
  
  console.log('AI setup page initialized');
}

// Entry point
document.addEventListener('DOMContentLoaded', initSetupPage);
