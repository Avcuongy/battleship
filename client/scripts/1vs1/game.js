// 1vs1 Game Page Logic (WebSocket-driven)
// - Connects WS with roomId/playerId
// - Renders boards and places player's fleet
// - Handles ATTACK_RESULT and GAME_OVER from server
// - Manages turn enable/disable and a timer (visual only)

(function () {
  const TURN_TIME = 20;

  const els = {
    player1Name: document.getElementById("player1Name"),
    player2Name: document.getElementById("player2Name"),
    turnIndicator: document.getElementById("turnIndicator"),
    timer: document.getElementById("timer"),
    playerBoard: document.getElementById("playerBoard"),
    enemyBoard: document.getElementById("enemyBoard"),
    playerBoardContainer: document.getElementById("playerBoardContainer"),
    enemyBoardContainer: document.getElementById("enemyBoardContainer"),
  };

  const state = {
    roomId: null,
    playerId: null,
    playerName: null,
    isMyTurn: false,
    isGameOver: false,
    timerId: null,
    remaining: TURN_TIME,
  };

  function qsParam(name) {
    const url = new URL(window.location.href);
    return url.searchParams.get(name);
  }

  function setNames(p1, p2) {
    if (els.player1Name) els.player1Name.textContent = p1 || "Me";
    if (els.player2Name) els.player2Name.textContent = p2 || "Opponent";
  }

  function setTurn(isMyTurn) {
    // Only reset/restart timer when turn actually changes.
    const next = !!isMyTurn;
    const prev = state.isMyTurn;
    const turnChanged = prev !== next;
    state.isMyTurn = next;
    if (els.turnIndicator) {
      els.turnIndicator.textContent = isMyTurn
        ? "Your Turn"
        : "Enemy's Turn";
      els.turnIndicator.classList.toggle("your", isMyTurn);
      els.turnIndicator.classList.toggle("enemy", !isMyTurn);
    }
    // Active highlight on board containers
    if (els.enemyBoardContainer) {
      els.enemyBoardContainer.classList.toggle("active", isMyTurn);
    }
    if (els.playerBoardContainer) {
      els.playerBoardContainer.classList.toggle("active", !isMyTurn);
    }

    // Enable/disable enemy clicks (always re-apply to recover after we temporarily disabled on fire)
    Board.disableAttacks("enemyBoard");
    if (isMyTurn) {
      Board.enableAttacks("enemyBoard", (pos) => onAttackCell(pos));
    }

    // Timer behavior:
    // - Do NOT reset when a shot is a HIT (turn stays the same) → keep counting
    // - Reset only when the turn actually changes (MISS or timeout)
    if (turnChanged) {
      startTimer();
    }
  }

  function startTimer() {
    stopTimer();
    state.remaining = TURN_TIME;
    updateTimer();
    state.timerId = setInterval(() => {
      state.remaining -= 1;
      updateTimer();
      if (state.remaining <= 0) {
        handleTimeout();
      }
    }, 1000);
  }

  function stopTimer() {
    if (state.timerId) {
      clearInterval(state.timerId);
      state.timerId = null;
    }
  }

  function updateTimer() {
    if (!els.timer) return;
    els.timer.textContent = `${state.remaining}s`;
    if (state.remaining <= 5) els.timer.style.color = "#ff1744";
    else if (state.remaining <= 10) els.timer.style.color = "#ff9800";
    else els.timer.style.color = "#4caf50";
  }

  function handleTimeout() {
    stopTimer();
    // Visual-only timeout: lock input until server advances turn
    if (state.isGameOver) return;
    Board.disableAttacks("enemyBoard");
    // We wait for server to send next ATTACK_RESULT (e.g., opponent move) to toggle turns
  }

  function onAttackCell(position) {
    if (!state.isMyTurn || state.isGameOver) return;
    try {
      WSManager.sendAttack(position);
      // Prevent multi-clicks until server responds
      Board.disableAttacks("enemyBoard");
      // Do not update board immediately; wait for server ATTACK_RESULT for source of truth
    } catch (err) {
      console.error("Failed to send attack:", err);
    }
  }

  function applyAttackVisual(targetBoardId, position, result) {
    if (!position) return;
    if (result === Protocol.RESULT.HIT || result === Protocol.RESULT.SUNK) {
      Board.markHit(targetBoardId, position);
    } else if (result === Protocol.RESULT.MISS) {
      Board.markMiss(targetBoardId, position);
    }
  }

  function onAttackResult(msg) {
    // msg: { attacker, position, result, shipType, nextTurn }
    const iAttacked = msg.attacker === state.playerId;

    if (iAttacked) {
      // Update enemy board
      applyAttackVisual("enemyBoard", msg.position, msg.result);
    } else {
      // Opponent attacked me
      applyAttackVisual("playerBoard", msg.position, msg.result);
    }

    // Turn handoff
    const nextIsMine = msg.nextTurn === state.playerId;
    setTurn(nextIsMine);
  }

  function onGameOver(msg) {
    state.isGameOver = true;
    stopTimer();
    const iWon = msg.winner === state.playerId;
    try {
      Storage.updateStats(!!iWon);
    } catch (_) {}
    // Show modal like AI mode
    const modal = document.getElementById("gameOverModal");
    const title = document.getElementById("modalTitle");
    const playerNameEl = document.getElementById("modalPlayerName");
    if (modal && title && playerNameEl) {
      if (iWon) {
        title.textContent = "WIN";
        playerNameEl.textContent = state.playerName || "You";
        modal.classList.add("win");
      } else {
        title.textContent = "LOSE";
        const enemyName = els.player2Name?.textContent || "Enemy";
        playerNameEl.textContent = enemyName;
        modal.classList.add("lose");
      }
      modal.style.display = "flex"; // CSS defaults to none
    }
  }

  function registerWsHandlers() {
    WSManager.on(Protocol.SERVER_MSG.ATTACK_RESULT, onAttackResult);
    WSManager.on(Protocol.SERVER_MSG.GAME_OVER, onGameOver);
    WSManager.on("disconnect", ({ code, reason }) => {
      alert(
        `Mất kết nối (code: ${code}${reason ? `, reason: ${reason}` : ""}).`
      );
      window.location.href = "/pages/1vs1/entry.html";
    });
  }

  async function primeUiFromRoom() {
    try {
      const room = await API.getRoomState(state.roomId);
      if (room) {
        // Names (support both grr* from API and plain fallback)
        const p1 = room.grrPlayer1Name || room.player1Name || state.playerName || 'Me';
        const p2 = room.grrPlayer2Name || room.player2Name || 'Opponent';
        setNames(p1, p2);

        // Determine initial turn if provided (prefer grrCurrentTurn from API)
        const ct = room.grrCurrentTurn || room.currentTurn;
        if (ct) {
          setTurn(ct === state.playerId);
        } else {
          // Fallback: assume player1 starts (backend sets player1 as first turn)
          const p1Id = room.grrPlayer1Id || room.player1Id;
          if (p1Id) setTurn(p1Id === state.playerId); else { setTurn(false); stopTimer(); }
        }
      }
    } catch (e) {
      console.warn("Failed to get room state; fallback UI", e);
      setNames(state.playerName, "Opponent");
      setTurn(false);
      stopTimer();
    }
  }

  function placeOwnFleet() {
    try {
      const fleet = Storage.getFleet();
      if (Array.isArray(fleet) && fleet.length) {
        fleet.forEach((ship) => Board.placeShip("playerBoard", ship));
      }
    } catch (e) {
      console.warn("No fleet found locally for rendering");
    }
  }

  async function init() {
    // Params
    state.roomId = qsParam("roomId") || Storage.getRoomId();
    state.playerId = qsParam("playerId") || Storage.getPlayerId();
    state.playerName = Storage.getPlayerName();

    if (!state.roomId || !state.playerId) {
      alert("Thiếu roomId hoặc playerId");
      window.location.href = "/pages/1vs1/entry.html";
      return;
    }

    // Render boards
    Board.render("playerBoard", true);
    Board.render("enemyBoard", false);
    placeOwnFleet();

    // Prefill names and turn from room state (best-effort)
    await primeUiFromRoom();

    // Connect WS
    try {
      await WSManager.connect(state.roomId, state.playerId);
      registerWsHandlers();
    } catch (e) {
      alert("Không thể kết nối WebSocket.");
      window.location.href = "/pages/1vs1/entry.html";
      return;
    }

    // Modal button
    document.getElementById("playAgainBtn")?.addEventListener("click", () => {
      window.location.href = "../home.html";
    });
  }

  document.addEventListener("DOMContentLoaded", init);
})();
