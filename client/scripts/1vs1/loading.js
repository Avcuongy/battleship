/* 1vs1 Loading Page Logic
   - Host: create room, show Room ID, wait for player2
   - Joiner: enter Room ID, join room, appear on right
   - WebSocket: connect after create/join
*/

(function () {
  const els = {
    player1Name: document.getElementById("player1Name"),
    player1Id: document.getElementById("player1Id"),
    player2Name: document.getElementById("player2Name"),
    player2Id: document.getElementById("player2Id"),
    roomIdText: document.getElementById("roomIdText"),
    joinInput: document.getElementById("joinRoomInput"),
    joinButton: document.getElementById("joinButton"),
    startButton: document.getElementById("startButton"),
    loadingRow: document.getElementById("loadingRow"),
    loadingText: document.getElementById("loadingText"),
  };

  const state = {
    isHost: false,
    roomId: null,
    playerId: null,
    playerName: null,
    pollTimer: null,
  };

  // Helpers
  function qsParam(name) {
    const url = new URL(window.location.href);
    return url.searchParams.get(name);
  }

  function setLoading(show, text) {
    // Gracefully no-op if loading UI elements are not present
    if (text) {
      try { console.log("[loading]", text); } catch (_) {}
    }
    if (els.loadingRow) {
      els.loadingRow.style.display = show ? "flex" : "none";
    }
    if (els.loadingText && text) {
      els.loadingText.textContent = text;
    }
  }

  function setPlayer1(id, name) {
    els.player1Id.textContent = `ID: ${id}`;
    els.player1Name.textContent = name || "Player 1";
  }

  function setPlayer2(id, name) {
    els.player2Id.textContent = `ID: ${id || "—"}`;
    els.player2Name.textContent = name || "Đang chờ...";
  }

  function setRoom(roomId) {
    els.roomIdText.textContent = roomId || "—";
  }

  async function ensurePlayer() {
    // Try load from storage; if missing, generate id via backend, name as Guest-XXXX
    let pid = Storage.getPlayerId();
    let pname = Storage.getPlayerName();

    if (!pid) {
      pid = await API.generatePlayerId();
      if (!pid) throw new Error("Cannot generate playerId");
      Storage.savePlayerId(pid);
    }
    if (!pname) {
      pname = `Guest-${pid}`;
      Storage.savePlayerName(pname);
    }

    state.playerId = pid;
    state.playerName = pname;
    setPlayer1(pid, pname);
  }

  async function hostFlow() {
    state.isHost = true;
    setLoading(true, "Đang tạo phòng...");
    try {
      const res = await API.createRoom(state.playerId, state.playerName);
      if (!res) throw new Error("Create room failed");
      state.roomId = res.crrRoomId;
      Storage.saveRoomId(state.roomId);
      setRoom(state.roomId);

      // Connect WS
      try {
        await WSManager.connect(state.roomId, state.playerId);
        console.log("WS connected as host");
      } catch (e) {
        console.warn("WS connect failed (host):", e);
      }

      // Start polling for player2 appearance
      setLoading(true, "Đang chờ người chơi tham gia...");
      state.pollTimer = setInterval(async () => {
        const room = await API.getRoomState(state.roomId);
        if (room && room.grrPlayer2Id) {
          setPlayer2(room.grrPlayer2Id, room.grrPlayer2Name || "Player 2");
          setLoading(false);
          // Enable start when both present
          els.startButton.disabled = false;
          clearInterval(state.pollTimer);
        }
      }, 1200);
    } catch (err) {
      console.error(err);
      setLoading(true, "Lỗi tạo phòng");
    }
  }

  async function joinFlow(roomId) {
    state.isHost = false;
    state.roomId = roomId;
    setRoom(roomId);

    setLoading(true, "Đang tham gia phòng...");
    const res = await API.joinRoom(roomId, state.playerId, state.playerName);
    if (!res) {
      setLoading(true, "Không thể tham gia phòng.");
      return;
    }

    // Connect WS as player2
    try {
      await WSManager.connect(state.roomId, state.playerId);
      console.log("WS connected as joiner");
    } catch (e) {
      console.warn("WS connect failed (joiner):", e);
    }

    // Update right box to show self (joiner appears on right)
    setPlayer2(state.playerId, state.playerName);
    setLoading(false, "Đã tham gia phòng");
    els.startButton.disabled = false; // optional: allow proceed
  }

  async function onConnectClick() {
    const val = (els.joinInput.value || "").trim();
    if (!val || val.length < 6) {
      setLoading(true, "ROOM ID không hợp lệ");
      return;
    }
    await joinFlow(val);
  }

  function wireEvents() {
    if (els.joinButton) {
      els.joinButton.addEventListener("click", onConnectClick);
    }
    if (els.joinInput) {
      els.joinInput.addEventListener("keydown", (e) => {
        if (e.key === "Enter") onConnectClick();
      });
    }

    els.startButton.addEventListener("click", () => {
      // Next page (ship placement for 1vs1), if exists
      // For now, go to gameplay page placeholder
      window.location.href = `/pages/1vs1/room.html?roomId=${state.roomId}`;
    });
  }

  async function init() {
    try {
      await ensurePlayer();
      wireEvents();

      // Detect from URL: if roomId present → joiner, else auto host (create room)
      const existingRoomId = qsParam("roomId");
      if (existingRoomId) {
        await joinFlow(existingRoomId);
      } else {
        await hostFlow();
      }
    } catch (e) {
      console.error("Init error:", e);
      setLoading(true, "Lỗi khởi tạo.");
    }
  }

  // Kickoff
  document.addEventListener("DOMContentLoaded", init);
})();
