// Utility functions for debugging and testing the player data system

class PlayerDebugUtils {
  static baseURL = 'http://localhost:3000';

  static async testLogin(nickname = 'TestUser', avatarIndex = 1) {
    try {
      const response = await fetch(`${this.baseURL}/api/login`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          nickname: nickname,
          avatarIndex: avatarIndex
        })
      });

      const result = await response.json();
      console.log('Login test result:', result);
      return result;
    } catch (error) {
      console.error('Login test error:', error);
      return null;
    }
  }

  static async testLogout(sessionId) {
    if (!sessionId) {
      const session = this.getSessionInfo();
      if (session && session.sessionId) {
        sessionId = session.sessionId;
      } else {
        console.log('No session ID provided or found');
        return;
      }
    }

    try {
      const response = await fetch(`${this.baseURL}/api/logout`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          sessionId: sessionId
        })
      });

      const result = await response.json();
      console.log('Logout test result:', result);
      return result;
    } catch (error) {
      console.error('Logout test error:', error);
      return null;
    }
  }

  static getSessionInfo() {
    const session = localStorage.getItem('battleship-session');
    if (session) {
      const parsed = JSON.parse(session);
      console.log('Current session:', parsed);
      return parsed;
    } else {
      console.log('No active session');
      return null;
    }
  }

  static clearSession() {
    localStorage.removeItem('battleship-session');
    console.log('Session cleared');
  }

  static async getCurrentPlayer() {
    const session = this.getSessionInfo();
    if (!session || !session.sessionId) {
      console.log('No valid session found');
      return null;
    }

    try {
      const response = await fetch(`${this.baseURL}/api/player/${session.sessionId}`);
      if (response.ok) {
        const player = await response.json();
        console.log('Current player:', player);
        return player;
      } else {
        console.log('Player not found or session expired');
        return null;
      }
    } catch (error) {
      console.error('Error getting current player:', error);
      return null;
    }
  }
}

// Make available globally for browser console debugging
window.PlayerDebugUtils = PlayerDebugUtils;

// Add some helpful console commands
console.log(`
Debug commands available:
- PlayerDebugUtils.testLogin('nickname', avatarIndex) - Test login with backend
- PlayerDebugUtils.testLogout(sessionId) - Test logout (auto-uses current session)
- PlayerDebugUtils.getSessionInfo() - Show current session info
- PlayerDebugUtils.clearSession() - Clear local session
- PlayerDebugUtils.getCurrentPlayer() - Get player data from backend
`);