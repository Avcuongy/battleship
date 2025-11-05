/**
 * Home Page Logic
 * Display player info and handle mode selection
 * No backend calls - pure UI navigation
 */

// ============================================================================
// Player Data Management
// ============================================================================

/**
 * Load player data from Storage
 * Redirect to initial page if not found
 */
function loadPlayerData() {
  const player = Storage.getPlayer();

  if (!player.playerId || !player.playerName) {
    console.warn("No player data found, redirecting to initial page");
    window.location.href = "./initial.html";
    return null;
  }

  console.log("Player loaded:", player);
  return player;
}

/**
 * Display player info in UI
 */
function displayPlayerInfo(player) {
  const nameElement = document.getElementById("playerName");
  const idElement = document.getElementById("playerId");

  if (nameElement) {
    nameElement.textContent = player.playerName;
  }

  if (idElement) {
    idElement.textContent = `ID: ${player.playerId}`;
  }
}

// ============================================================================
// Mode Selection Handlers
// ============================================================================

/**
 * Handle single player (AI) mode selection
 */
function handleSinglePlayerMode() {
  console.log("Selected: AI mode");

  // Save game mode to sessionStorage (will be used by loading page)
  sessionStorage.setItem("battleship-game-mode", "ai");

  // Navigate to AI loading page
  window.location.href = "./ai/loading.html";
}

/**
 * Handle two player (1vs1) mode selection
 */
function handleTwoPlayerMode() {
  console.log("Selected: 1vs1 mode");

  // Save game mode to sessionStorage
  sessionStorage.setItem("battleship-game-mode", "1vs1");

  // Navigate to 1vs1 entry page (pre-lobby) using absolute path to avoid relative issues
  window.location.href = "/pages/1vs1/entry.html";
}

// ============================================================================
// Event Listeners Setup
// ============================================================================

/**
 * Setup all event listeners for mode buttons
 */
function setupEventListeners() {
  const singlePlayerBtn = document.getElementById("singlePlayerBtn");
  const twoPlayerBtn = document.getElementById("twoPlayerBtn");

  if (singlePlayerBtn) {
    singlePlayerBtn.addEventListener("click", handleSinglePlayerMode);
  }

  if (twoPlayerBtn) {
    twoPlayerBtn.addEventListener("click", handleTwoPlayerMode);
  }
}

// ============================================================================
// Initialization
// ============================================================================

/**
 * Initialize home page
 */
function initHomePage() {
  console.log("Initializing home page...");

  // Load and display player data
  const player = loadPlayerData();
  if (!player) {
    return; // Redirected to initial page
  }

  displayPlayerInfo(player);

  // Setup event listeners
  setupEventListeners();

  console.log("Home page initialized");
}

// Entry point
document.addEventListener("DOMContentLoaded", initHomePage);
