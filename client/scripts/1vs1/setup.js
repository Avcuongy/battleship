// 1vs1 Setup Page Logic
// - Connects to WS using roomId/playerId from URL
// - Renders board and ships, validates placement
// - Sends Ready message with fleet over WS

(function () {
  const els = {
    player1Name: document.getElementById("player1Name"),
    player1Status: document.getElementById("player1Status"),
    player2Name: document.getElementById("player2Name"),
    player2Status: document.getElementById("player2Status"),
    readyBtn: document.getElementById("readyBtn"),
    resetBtn: document.getElementById("resetBtn"),
    board: document.getElementById("gameBoard"),
  };

  const state = {
    roomId: null,
    playerId: null,
    playerName: null,
  };

  function qsParam(name) {
    const url = new URL(window.location.href);
    return url.searchParams.get(name);
  }

  function setSelf(name) {
    if (els.player1Name) els.player1Name.textContent = name || "Me";
  }

  function setReadyStatus(isReady) {
    if (!els.player1Status) return;
    els.player1Status.textContent = isReady ? "Ready" : "Not Ready";
    els.player1Status.classList.toggle("ready", isReady);
    els.player1Status.classList.toggle("not-ready", !isReady);
  }

  function registerWsHandlers() {
    // When game starts from both ready (server-side validation), navigate to gameplay
    WSManager.on(Protocol.SERVER_MSG.GAME_START, () => {
      console.log("[WS] GAME_START on setup → navigating to gameplay");
      const target = `/pages/1vs1/game.html?roomId=${encodeURIComponent(
        state.roomId
      )}&playerId=${encodeURIComponent(state.playerId)}`;
      window.location.href = target;
    });

    // When a player becomes ready, update UI statuses
    WSManager.on(Protocol.SERVER_MSG.PLAYER_READY, (msg) => {
      const pid = msg.playerId;
      const isReady = !!msg.ready;
      console.log("[WS] PLAYER_READY", pid, isReady);
      if (pid === state.playerId) {
        setReadyStatus(isReady);
      } else {
        if (els.player2Status) {
          els.player2Status.textContent = isReady ? "Ready" : "Not Ready";
          els.player2Status.classList.toggle("ready", isReady);
          els.player2Status.classList.toggle("not-ready", !isReady);
        }
      }
    });

    // Disconnect handler (graceful)
    WSManager.on("disconnect", ({ code, reason }) => {
      alert(
        `Mất kết nối (code: ${code}${
          reason ? `, reason: ${reason}` : ""
        }). Quay về phòng vào lại.`
      );
      window.location.href = "/pages/1vs1/entry.html";
    });
  }

  function wireUI() {
    if (els.resetBtn) {
      els.resetBtn.addEventListener("click", (e) => {
        e.preventDefault();
        Ships.reset();
        setReadyStatus(false);
      });
    }
    if (els.readyBtn) {
      els.readyBtn.addEventListener("click", (e) => {
        e.preventDefault();
        const fleet = Ships.getFleet();
        if (!fleet || fleet.length !== 5) {
          alert("Not enough ships placed");
          return;
        }
        const v = Validation.validateFleet(fleet);
        if (!v.valid) {
          alert(v.errors.join("\n"));
          return;
        }
        // Persist fleet locally for game page rendering
        try {
          Storage.saveFleet(fleet);
        } catch (_) {}
        els.readyBtn.disabled = true;
        setReadyStatus(true);
        WSManager.sendReady(fleet);
      });
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

    setSelf(state.playerName || state.playerId);

    // Render board and ship palette ASAP (before network awaits)
    Board.render("gameBoard", true);
    // Ensure cells exist before initializing Ships (avoid race on slow paints)
    if (els.board && els.board.querySelectorAll(".cell").length === 0) {
      try {
        await new Promise(requestAnimationFrame);
      } catch (_) {}
    }
    Ships.init("gameBoard", "shipsSection");

    // Load room state to fill player2 name and ready states (STM-backed)
    try {
      const room = await API.getRoomState(state.roomId);
      if (room) {
        if (els.player2Name) {
          const p2n = room.grrPlayer2Name || room.player2Name || "—";
          els.player2Name.textContent = p2n || "—";
        }
        // Initial statuses if available
        if (typeof room.player1Ready === "boolean" && els.player1Status) {
          setReadyStatus(!!room.player1Ready);
        }
        if (typeof room.player2Ready === "boolean" && els.player2Status) {
          const rdy = !!room.player2Ready;
          els.player2Status.textContent = rdy ? "Ready" : "Not Ready";
          els.player2Status.classList.toggle("ready", rdy);
          els.player2Status.classList.toggle("not-ready", !rdy);
        }
      }
    } catch (_) {}

    // Connect WebSocket
    try {
      console.log(
        `[SETUP] Connecting WS: roomId=${state.roomId}, playerId=${state.playerId}`
      );
      await WSManager.connect(state.roomId, state.playerId);
      registerWsHandlers();
    } catch (e) {
      alert("Không thể kết nối WebSocket.");
      window.location.href = "/pages/1vs1/entry.html";
      return;
    }

    wireUI();
  }

  document.addEventListener("DOMContentLoaded", init);
})();
