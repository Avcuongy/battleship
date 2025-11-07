// AI Game Page (backend-driven)
const TURN_TIME = 20;

let gameState = {
  gameId: null,
  playerId: null,
  playerName: null,
  playerFleet: [],
  isPlayerTurn: true,
  isGameOver: false,
  awaitingResponse: false,
};

let turnTimer = null;
let remainingTime = TURN_TIME;

function init() {
  console.log("AI Game - Init");

  const gameId = Storage.getGameId();
  const player = Storage.getPlayer();
  const fleet = Storage.getFleet();

  if (!gameId || !player.playerId || !fleet || fleet.length !== 5) {
    console.error("Missing data");
    alert("Missing data !");
    window.location.href = "./setup.html";
    return;
  }

  gameState.gameId = gameId;
  gameState.playerId = player.playerId;
  gameState.playerName = player.playerName;
  gameState.playerFleet = fleet;

  console.log("Loaded:", {
    gameId,
    player: player.playerName,
    fleet: fleet.length,
  });

  document.getElementById("player1Name").textContent = player.playerName;
  document.getElementById("player2Name").textContent = "AI";

  Board.render("playerBoard", true);
  Board.render("enemyBoard", false);

  fleet.forEach((ship) => Board.placeShip("playerBoard", ship));

  // Enable enemy board clicks
  enableEnemyBoardClicks();

  // Setup modal button
  document.getElementById("playAgainBtn")?.addEventListener("click", () => {
    window.location.href = "../home.html";
  });

  startTimer();
  updateFireIndicator(); // Set to default "FIRE"

  console.log("Game ready");
}

function convertFleet(fleet) {
  const sizes = {
    Carrier: 5,
    Battleship: 4,
    Cruiser: 3,
    Submarine: 3,
    Destroyer: 2,
  };
  return fleet.map((ship) => {
    const size = sizes[ship.shipType];
    const positions = [];
    for (let i = 0; i < size; i++) {
      if (ship.shipOrientation === "Horizontal") {
        positions.push({
          posRow: ship.shipPosition.posRow,
          posCol: ship.shipPosition.posCol + i,
        });
      } else {
        positions.push({
          posRow: ship.shipPosition.posRow + i,
          posCol: ship.shipPosition.posCol,
        });
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
  const el = document.getElementById("timer");
  if (!el) return;
  el.textContent = remainingTime + "s";
  if (remainingTime <= 5) el.style.color = "#ff1744";
  else if (remainingTime <= 10) el.style.color = "#ff9800";
  else el.style.color = "#4caf50";
}

function handleTimeout() {
  console.log("Timer timeout = treat as MISS");
  stopTimer();

  if (gameState.isGameOver) return;
  // On timeout, we treat as a MISS by firing at a guaranteed-miss cell (odd row)
  // so backend advances AI move and returns control back to player after AI attacks.
  void (async () => {
    try {
      gameState.awaitingResponse = true;
      const missPos = findGuaranteedMissCell();
      if (!missPos) {
        // Fallback: pick any empty cell
        const any = findAnyEmptyEnemyCell();
        if (!any) return;
        // Not guaranteed, but proceed
        const resp = await API.attackAI(gameState.gameId, any);
        await handleTimeoutResponse(resp);
        return;
      }
      const response = await API.attackAI(gameState.gameId, missPos);
      await handleTimeoutResponse(response);
    } catch (e) {
      console.error("Timeout handling failed:", e);
    } finally {
      gameState.awaitingResponse = false;
    }
  })();
}

// Handle response path for timeout-triggered attack
async function handleTimeoutResponse(response) {
  if (!response) return;
  const p = response.aarPlayerResult;
  if (p) {
    if (p.arResult === "miss") {
      Board.markMiss("enemyBoard", p.arPosition);
      showNotification("MISS");
    } else {
      // Shouldn't happen with guaranteed miss, but handle gracefully
      if (p.arResult === "hit" || p.arResult === "sunk") {
        Board.markHit("enemyBoard", p.arPosition);
      }
    }
  }

  if (response.aarGameOver) {
    endGame(response.aarWinner === "player");
    return;
  }

  const a = response.aarAiResult;
  if (a) {
    if (a.arResult === "hit" || a.arResult === "sunk") {
      Board.markHit("playerBoard", a.arPosition);
      if (a.arResult === "sunk" && a.arShipType) {
        showNotification(`AI SUNK your ${a.arShipType}!`);
      }
    } else if (a.arResult === "miss") {
      Board.markMiss("playerBoard", a.arPosition);
    }
  }

  // After AI move, give turn back to player and reset timer
  switchToPlayerTurn();
}

// Find an enemy cell that is guaranteed to be a MISS against default AI fleet
function findGuaranteedMissCell() {
  const enemyBoard = document.getElementById("enemyBoard");
  if (!enemyBoard) return null;
  const cells = enemyBoard.querySelectorAll(".cell");
  for (const cell of cells) {
    const row = parseInt(cell.dataset.row);
    const col = parseInt(cell.dataset.col);
    const state = cell.dataset.state || "empty";
    if (state !== "empty") continue;
    // AI default fleet occupies only even rows (0,2,4,6,8) horizontally at low columns.
    // Any odd row is guaranteed miss.
    if (row % 2 === 1) {
      return { posRow: row, posCol: col };
    }
  }
  return null;
}

// Fallback: find any empty enemy cell
function findAnyEmptyEnemyCell() {
  const enemyBoard = document.getElementById("enemyBoard");
  if (!enemyBoard) return null;
  const cells = enemyBoard.querySelectorAll(".cell");
  for (const cell of cells) {
    const state = cell.dataset.state || "empty";
    if (state === "empty") {
      return { posRow: parseInt(cell.dataset.row), posCol: parseInt(cell.dataset.col) };
    }
  }
  return null;
}

// Enable clicks on enemy board
function enableEnemyBoardClicks() {
  const enemyBoard = document.getElementById("enemyBoard");
  if (!enemyBoard) return;

  enemyBoard.addEventListener("click", handleEnemyBoardClick);
}

// Handle click on enemy board
async function handleEnemyBoardClick(e) {
  if (gameState.isGameOver || !gameState.isPlayerTurn || gameState.awaitingResponse) return;

  const cell = e.target.closest(".cell");
  if (!cell) return;

  const row = parseInt(cell.dataset.row);
  const col = parseInt(cell.dataset.col);

  if (isNaN(row) || isNaN(col)) return;

  // Avoid re-attacking the same cell
  if (cell.dataset.state && cell.dataset.state !== "empty") return;

  // Call backend to process attack
  try {
    gameState.awaitingResponse = true;
    const response = await API.attackAI(gameState.gameId, { posRow: row, posCol: col });
    if (!response) return;

    // Player result
    const p = response.aarPlayerResult;
    if (p) {
      if (p.arResult === "hit" || p.arResult === "sunk") {
        Board.markHit("enemyBoard", p.arPosition);
        if (p.arResult === "sunk" && p.arShipType) {
          showNotification(`SUNK ${p.arShipType}`);
        } else {
          showNotification("HIT");
        }
      } else if (p.arResult === "miss") {
        Board.markMiss("enemyBoard", p.arPosition);
        showNotification("MISS");
      }
    }

    // Game over?
    if (response.aarGameOver) {
      endGame(response.aarWinner === "player");
      return;
    }

    // If player missed, stop timer and AI has moved in the same response
    if (p && p.arResult === "miss") {
      stopTimer();
      gameState.isPlayerTurn = false;
      const a = response.aarAiResult;
      if (a) {
        if (a.arResult === "hit" || a.arResult === "sunk") {
          Board.markHit("playerBoard", a.arPosition);
          if (a.arResult === "sunk" && a.arShipType) {
            showNotification(`AI SUNK your ${a.arShipType}!`);
          }
        } else if (a.arResult === "miss") {
          Board.markMiss("playerBoard", a.arPosition);
        }
      }
      
      // After AI move (no game over), give turn back to player
      switchToPlayerTurn();
    } else {
      // Player hit: continues, do not reset timer
      // Keep current timer running; just ensure enemy board is clickable
      gameState.isPlayerTurn = true;
    }
  } catch (err) {
    console.error("AI attack failed:", err);
  } finally {
    gameState.awaitingResponse = false;
  }
}

// Switch to AI turn
function switchToAITurn() {
  console.log("Switch to AI turn");
  stopTimer();

  gameState.isPlayerTurn = false;
  // No indicator update needed - keep showing FIRE
  // With backend AI, the AI move is returned in the same response when player misses.
  // So here we don't proactively attack; we only manage timing.
}

// Switch to player turn
function switchToPlayerTurn() {
  console.log("Switch to player turn");

  gameState.isPlayerTurn = true;
  updateFireIndicator(); // Reset to "FIRE"
  startTimer(); // Reset timer for new turn
}

// Update fire indicator to default "FIRE"
function updateFireIndicator() {
  const indicator = document.getElementById("fireIndicator");
  if (!indicator) return;

  indicator.textContent = "FIRE";
  indicator.style.color = "#2a5298";
}

// Show notification (MISS / HIT / SUNK + shipType)
function showNotification(message) {
  const indicator = document.getElementById("fireIndicator");
  if (!indicator) return;

  indicator.textContent = message;
  indicator.style.color = "#2a5298";

  setTimeout(() => {
    updateFireIndicator(); // Reset to "FIRE"
  }, 1500);
}

// End game
async function endGame(playerWon) {
  console.log("Game over - Player won:", playerWon);

  gameState.isGameOver = true;
  stopTimer();

  // Update stats (saves to server: data/players/)
  await Storage.updateStats(playerWon);

  // Show modal
  const modal = document.getElementById("gameOverModal");
  const title = document.getElementById("modalTitle");
  const playerNameEl = document.getElementById("modalPlayerName");

  if (modal && title && playerNameEl) {
    if (playerWon) {
      title.textContent = "WIN";
      playerNameEl.textContent = gameState.playerName;
      modal.classList.add("win");
    } else {
      title.textContent = "LOSE";
      playerNameEl.textContent = "AI";
      modal.classList.add("lose");
    }

    modal.style.display = "flex";
  }
}

function handleTimeout() {
  console.log("Timer timeout - treat as MISS");
  stopTimer();

  if (gameState.isGameOver) return;

  // Switch to AI turn
  switchToAITurn();
}

document.addEventListener("DOMContentLoaded", init);

window.addEventListener("beforeunload", async () => {
  if (!gameState.isGameOver) {
    await Storage.updateStats(false);
  }
});
