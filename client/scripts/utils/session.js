/**
 * Session Manager for BattleShip Game
 * Handles localStorage persistence for player session and game state
 */

const SESSION_KEY = 'battleship-session';
const GAME_STATE_KEY = 'battleship-game-state';

class SessionManager {
  /**
   * Save player session to localStorage
   * @param {object} player - Player data {playerId, name, stats}
   */
  static saveSession(player) {
    const sessionData = {
      player: {
        id: player.playerId,
        name: player.name,
        stats: player.stats
      },
      timestamp: Date.now()
    };
    localStorage.setItem(SESSION_KEY, JSON.stringify(sessionData));
  }

  /**
   * Get current session
   * @returns {object|null} Session data or null
   */
  static getSession() {
    const data = localStorage.getItem(SESSION_KEY);
    if (!data) return null;
    
    try {
      return JSON.parse(data);
    } catch (e) {
      console.error('Failed to parse session:', e);
      return null;
    }
  }

  /**
   * Clear session
   */
  static clearSession() {
    localStorage.removeItem(SESSION_KEY);
    localStorage.removeItem(GAME_STATE_KEY);
  }

  /**
   * Save game state (room, ships, etc.)
   * @param {object} gameState - Game state data
   */
  static saveGameState(gameState) {
    localStorage.setItem(GAME_STATE_KEY, JSON.stringify(gameState));
  }

  /**
   * Get game state
   * @returns {object|null} Game state or null
   */
  static getGameState() {
    const data = localStorage.getItem(GAME_STATE_KEY);
    if (!data) return null;
    
    try {
      return JSON.parse(data);
    } catch (e) {
      console.error('Failed to parse game state:', e);
      return null;
    }
  }

  /**
   * Update game state partially
   * @param {object} updates - Partial updates to merge
   */
  static updateGameState(updates) {
    const current = this.getGameState() || {};
    const updated = { ...current, ...updates };
    this.saveGameState(updated);
  }

  /**
   * Clear game state only (keep session)
   */
  static clearGameState() {
    localStorage.removeItem(GAME_STATE_KEY);
  }

  /**
   * Check if user is logged in
   * @returns {boolean}
   */
  static isLoggedIn() {
    const session = this.getSession();
    return session !== null && session.player !== null;
  }

  /**
   * Get player ID
   * @returns {string|null}
   */
  static getPlayerId() {
    const session = this.getSession();
    return session?.player?.id || null;
  }

  /**
   * Get player name
   * @returns {string|null}
   */
  static getPlayerName() {
    const session = this.getSession();
    return session?.player?.name || null;
  }
}

// Export for use in other files
if (typeof module !== 'undefined' && module.exports) {
  module.exports = SessionManager;
}
