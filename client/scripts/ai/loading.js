/**
 * AI Loading Page
 * Display player info and navigate to setup
 */

// ============================================================================
// Player Info Display
// ============================================================================

/**
 * Load and display player info
 */
function displayPlayerInfo() {
  const player = Storage.getPlayer();
  
  if (!player.playerId || !player.playerName) {
    console.warn('No player data found, redirecting to home');
    window.location.href = '../home.html';
    return;
  }

  // Display player 1 (user) info
  const player1Name = document.getElementById('player1Name');
  const player1Id = document.getElementById('player1Id');
  
  if (player1Name) {
    player1Name.textContent = player.playerName;
  }
  
  if (player1Id) {
    player1Id.textContent = `ID: ${player.playerId}`;
  }

  console.log('Player info displayed:', player);
}

// ============================================================================
// Navigation
// ============================================================================

/**
 * Handle start button click
 * Navigate to setup page
 */
function handleStartClick() {
  console.log('Starting AI game setup...');
  
  // Navigate to setup page
  window.location.href = './setup.html';
}

// ============================================================================
// Event Listeners
// ============================================================================

/**
 * Setup event listeners
 */
function setupEventListeners() {
  const startButton = document.getElementById('startButton');
  
  if (startButton) {
    // Enable button immediately (no waiting needed for AI)
    startButton.disabled = false;
    
    startButton.addEventListener('click', handleStartClick);
  }
}

// ============================================================================
// Initialization
// ============================================================================

/**
 * Initialize loading page
 */
function initLoadingPage() {
  console.log('Initializing AI loading page...');
  
  // Check game mode
  const gameMode = sessionStorage.getItem('battleship-game-mode');
  if (gameMode !== 'ai') {
    console.warn('Invalid game mode, redirecting to home');
    window.location.href = '../home.html';
    return;
  }

  // Display player info
  displayPlayerInfo();
  
  // Setup event listeners
  setupEventListeners();
  
  console.log('AI loading page initialized');
}

// Entry point
document.addEventListener('DOMContentLoaded', initLoadingPage);
