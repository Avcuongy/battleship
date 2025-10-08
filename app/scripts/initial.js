class BattleShipClient {
  constructor() {
    this.baseURL = 'http://localhost:3000';
    this.avatars = [
      '../../assets/images/captain.jpg',
      '../../assets/images/daden.jpg',
      '../../assets/images/female.jpg'
    ];
    this.currentAvatarIndex = 1;
    this.init();
  }

  init() {
    this.setupEventListeners();
    this.updateAvatarDisplay();
  }

  setupEventListeners() {
    // Avatar refresh
    document.getElementById('refreshAvatar').addEventListener('click', () => {
      this.currentAvatarIndex = (this.currentAvatarIndex + 1) % this.avatars.length;
      this.updateAvatarDisplay();
    });

    // Start button
    document.getElementById('startButton').addEventListener('click', () => {
      this.handleLogin();
    });

    // Nickname validation
    document.getElementById('nicknameInput').addEventListener('input', (e) => {
      this.validateNickname(e.target.value);
    });
  }

  updateAvatarDisplay() {
    const img = document.querySelector('#avatar img');
    img.src = this.avatars[this.currentAvatarIndex];
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
          nickname: nickname,
          avatarIndex: this.currentAvatarIndex
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