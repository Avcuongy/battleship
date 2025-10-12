class HomePageClient {
  constructor() {
    this.loadPlayerData();
    this.setupCleanup();
  }

  loadPlayerData() {
    // Lấy từ localStorage (NOT từ file JSON)
    const sessionData = JSON.parse(localStorage.getItem('battleship-session'));
    
    if (!sessionData || !sessionData.player) {
      // Không có session → redirect về initial
      window.location.href = '/templates/initial.html';
      return;
    }

    this.displayPlayer(sessionData.player);
  }

  displayPlayer(player) {
    // Hiển thị id, name (không có avatar)
    document.getElementById('playerName').textContent = player.name;
    document.getElementById('playerId').textContent = `ID: ${player.id}`;
  }

  setupCleanup() {
    // Auto logout khi đóng web
    window.addEventListener('beforeunload', () => {
      this.logout();
    });
  }

  async logout() {
    const sessionData = JSON.parse(localStorage.getItem('battleship-session'));
    if (sessionData?.sessionId) {
      try {
        await fetch('http://localhost:3000/api/logout', {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({ sessionId: sessionData.sessionId })
        });
      } catch (error) {
        console.error('Logout error:', error);
      }
    }
    localStorage.removeItem('battleship-session');
  }
}

document.addEventListener('DOMContentLoaded', () => {
  new HomePageClient();
});