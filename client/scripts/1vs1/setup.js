// 1vs1 Setup Page Logic
// - Connects to WS using roomId/playerId from URL
// - Renders board and ships, validates placement
// - Sends Ready message with fleet over WS

(function () {
	const els = {
		player1Name: document.getElementById('player1Name'),
		player1Status: document.getElementById('player1Status'),
		player2Name: document.getElementById('player2Name'),
		player2Status: document.getElementById('player2Status'),
		readyBtn: document.getElementById('readyBtn'),
		resetBtn: document.getElementById('resetBtn'),
		board: document.getElementById('gameBoard'),
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
		if (els.player1Name) els.player1Name.textContent = name || 'Me';
	}

	function setReadyStatus(isReady) {
		if (!els.player1Status) return;
		els.player1Status.textContent = isReady ? 'Ready' : 'Not Ready';
		els.player1Status.classList.toggle('ready', isReady);
		els.player1Status.classList.toggle('not-ready', !isReady);
	}

	function registerWsHandlers() {
		// When game starts from both ready (server-side validation), navigate to gameplay
		WSManager.on(Protocol.SERVER_MSG.GAME_START, () => {
			console.log('[WS] GAME_START on setup → navigating to gameplay');
			const target = `/pages/1vs1/game.html?roomId=${encodeURIComponent(state.roomId)}&playerId=${encodeURIComponent(state.playerId)}`;
			window.location.href = target;
		});

		// Disconnect handler (graceful)
		WSManager.on('disconnect', ({ code, reason }) => {
			alert(`Mất kết nối (code: ${code}${reason ? `, reason: ${reason}` : ''}). Quay về phòng vào lại.`);
			window.location.href = '/pages/1vs1/entry.html';
		});
	}

	function wireUI() {
		if (els.resetBtn) {
			els.resetBtn.addEventListener('click', (e) => {
				e.preventDefault();
				Ships.reset();
				setReadyStatus(false);
			});
		}
		if (els.readyBtn) {
			els.readyBtn.addEventListener('click', (e) => {
				e.preventDefault();
				const fleet = Ships.getFleet();
				if (!fleet || fleet.length !== 5) {
					alert('Hãy đặt đủ 5 tàu.');
					return;
				}
				const v = Validation.validateFleet(fleet);
				if (!v.valid) {
					alert(v.errors.join('\n'));
					return;
				}
				els.readyBtn.disabled = true;
				setReadyStatus(true);
				WSManager.sendReady(fleet);
			});
		}
	}

	async function init() {
		// Params
		state.roomId = qsParam('roomId') || Storage.getRoomId();
		state.playerId = qsParam('playerId') || Storage.getPlayerId();
		state.playerName = Storage.getPlayerName();

		if (!state.roomId || !state.playerId) {
			alert('Thiếu roomId hoặc playerId');
			window.location.href = '/pages/1vs1/entry.html';
			return;
		}

		setSelf(state.playerName || state.playerId);

		// Render board and ship palette
		Board.render('gameBoard', true);
		Ships.init('gameBoard', null, (fleet) => {
			if (els.readyBtn) els.readyBtn.disabled = (fleet.length !== 5);
		});

		// Connect WebSocket
		try {
			console.log(`[SETUP] Connecting WS: roomId=${state.roomId}, playerId=${state.playerId}`);
			await WSManager.connect(state.roomId, state.playerId);
			registerWsHandlers();
		} catch (e) {
			alert('Không thể kết nối WebSocket.');
			window.location.href = '/pages/1vs1/entry.html';
			return;
		}

		wireUI();
	}

	document.addEventListener('DOMContentLoaded', init);
})();
