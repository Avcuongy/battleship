(function () {
  const els = {
    playerName: document.getElementById('playerName'),
    playerId: document.getElementById('playerId'),
    roomIdText: document.getElementById('roomIdText'),
    joinInput: document.getElementById('joinRoomInput'),
    joinButton: document.getElementById('joinButton'),
    hostButton: document.getElementById('hostButton'),
    hintText: document.getElementById('hintText'),
  };

  const state = {
    playerId: null,
    playerName: null,
    activeRoomId: null,
  };

  function setPlayerInfo(id, name) {
    els.playerId.textContent = `ID: ${id}`;
    els.playerName.textContent = name;
  }

  function setRoomId(rid) {
    els.roomIdText.textContent = rid || '—';
  }

  function setHint(text) {
    els.hintText.textContent = text || '';
  }

  async function ensurePlayer() {
    let pid = Storage.getPlayerId();
    let pname = Storage.getPlayerName();

    if (!pid) {
      pid = await API.generatePlayerId();
      if (!pid) throw new Error('Không thể tạo playerId');
      Storage.savePlayerId(pid);
    }
    if (!pname) {
      pname = `Guest-${pid}`;
      Storage.savePlayerName(pname);
    }

    state.playerId = pid;
    state.playerName = pname;
    setPlayerInfo(pid, pname);
  }

  async function checkActiveRoom() {
    try {
      const res = await API.getActiveRoom();
      if (res && res.garStatus === 'active' && res.garRoomId) {
        state.activeRoomId = res.garRoomId;
        setRoomId(res.garRoomId);
        setHint('Đã có phòng đang hoạt động. Vui lòng dùng ROOM ID để tham gia.');
        if (els.hostButton) {
          els.hostButton.disabled = true;
          els.hostButton.textContent = 'Đang có phòng';
        }
      } else {
        setRoomId('—');
        setHint('Chưa có phòng. Bạn có thể bấm Tạo phòng hoặc nhập ROOM ID để kết nối.');
        if (els.hostButton) {
          els.hostButton.disabled = false;
          els.hostButton.textContent = 'Tạo phòng';
        }
      }
    } catch (e) {
      console.warn('getActiveRoom failed', e);
    }
  }

  async function onHostClick() {
    if (state.activeRoomId) return; // safety
    // Navigate to loading with host flag; loading will create the room and connect
    window.location.href = './loading.html?host=1';
  }

  async function onJoinClick() {
    const rid = (els.joinInput.value || '').trim();
    if (!rid || rid.length < 6) {
      alert('ROOM ID không hợp lệ');
      return;
    }
    window.location.href = `./loading.html?roomId=${encodeURIComponent(rid)}`;
  }

  function wireEvents() {
    if (els.hostButton) els.hostButton.addEventListener('click', onHostClick);
    if (els.joinButton) els.joinButton.addEventListener('click', onJoinClick);
    if (els.joinInput) {
      els.joinInput.addEventListener('keydown', (e) => {
        if (e.key === 'Enter') onJoinClick();
      });
    }
  }

  async function init() {
    try {
      await ensurePlayer();
      wireEvents();
      await checkActiveRoom();
    } catch (e) {
      console.error('Entry init failed:', e);
    }
  }

  document.addEventListener('DOMContentLoaded', init);
})();
