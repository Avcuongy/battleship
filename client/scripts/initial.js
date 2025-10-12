class BattleShipClient {
  constructor() {
    this.baseURL = 'http://localhost:3000';
    this.init();
  }

  init() {
    this.setupEventListeners();
  }

  setupEventListeners() {
    // Start button
    document.getElementById('startButton').addEventListener('click', () => {
      this.handleLogin();
    });

    // Nickname validation
    document.getElementById('nicknameInput').addEventListener('input', (e) => {
      this.validateNickname(e.target.value);
    });
  }

  validateNickname(nickname) {
    const isValid = /^[a-zA-Z0-9_-]{2,30}$/.test(nickname);
    const button = document.getElementById('startButton');
    const input = document.getElementById('nicknameInput');

    if (isValid) {
      button.disabled = false;
      input.style.borderColor = '#4caf50';
    } else {
      button.disabled = true;
      input.style.borderColor = '#d32f2f';
    }
    return isValid;
  }

  async handleLogin() {
    const nickname = document.getElementById('nicknameInput').value.trim();

    if (!this.validateNickname(nickname)) {
      alert('Biệt danh không hợp lệ!');
      return;
    }

    try {
      const response = await fetch(`${this.baseURL}/api/login`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          nickname: nickname
        })
      });

      const result = await response.json();

      if (result.success) {
        // Store session with player data from backend
        localStorage.setItem('battleship-session', JSON.stringify({
          sessionId: result.sessionId,
          player: result.player
        }));

        // Redirect to home
        window.location.href = '/templates/home.html';
      } else {
        alert('Đăng nhập thất bại: ' + result.message);
      }
    } catch (error) {
      console.error('Login error:', error);
      alert('Lỗi kết nối server!');
    }
  }
}

// Initialize when DOM loaded
document.addEventListener('DOMContentLoaded', () => {
  new BattleShipClient();
});