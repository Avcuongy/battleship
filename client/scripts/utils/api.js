/**
 * API Client for BattleShip Game
 * Handles all HTTP requests to backend server
 */

const API_BASE_URL = 'http://localhost:3000/api';

class BattleShipAPI {
  /**
   * Login player
   * @param {string} name - Player name
   * @returns {Promise<{playerId: string, name: string, stats: object}>}
   */
  static async login(name) {
    const response = await fetch(`${API_BASE_URL}/login`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ name })
    });

    if (!response.ok) {
      throw new Error(`Login failed: ${response.statusText}`);
    }

    return await response.json();
  }

  /**
   * Create game room
   * @param {string} playerId - Player ID
   * @param {string} gameMode - "AI" or "1vs1"
   * @returns {Promise<{roomId: string, gameMode: string, status: string}>}
   */
  static async createRoom(playerId, gameMode) {
    const response = await fetch(`${API_BASE_URL}/room/create`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ playerId, gameMode })
    });

    if (!response.ok) {
      throw new Error(`Create room failed: ${response.statusText}`);
    }

    return await response.json();
  }

  /**
   * Set player ready status
   * @param {string} roomId - Room ID
   * @param {string} playerId - Player ID
   * @param {boolean} ready - Ready status
   * @returns {Promise<{status: string, canStart: boolean}>}
   */
  static async setReady(roomId, playerId, ready) {
    const response = await fetch(`${API_BASE_URL}/room/ready`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ roomId, playerId, ready })
    });

    if (!response.ok) {
      throw new Error(`Set ready failed: ${response.statusText}`);
    }

    return await response.json();
  }

  /**
   * Setup ships on board
   * @param {string} roomId - Room ID
   * @param {string} playerId - Player ID
   * @param {Array} ships - Array of ship objects
   * @returns {Promise<{status: string}>}
   */
  static async setupShips(roomId, playerId, ships) {
    const response = await fetch(`${API_BASE_URL}/game/setup`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ roomId, playerId, ships })
    });

    if (!response.ok) {
      throw new Error(`Setup ships failed: ${response.statusText}`);
    }

    return await response.json();
  }

  /**
   * Make a move (shoot at position)
   * @param {string} roomId - Room ID
   * @param {string} playerId - Player ID
   * @param {Array<number>} position - [row, col]
   * @returns {Promise<{result: string, position: Array, nextTurn: string}>}
   */
  static async makeMove(roomId, playerId, position) {
    const response = await fetch(`${API_BASE_URL}/game/move`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ roomId, playerId, position })
    });

    if (!response.ok) {
      throw new Error(`Make move failed: ${response.statusText}`);
    }

    return await response.json();
  }

  /**
   * Complete game (report winner)
   * @param {string} roomId - Room ID
   * @param {string} winnerId - Winner player ID
   * @param {string} loserId - Loser player ID
   * @returns {Promise<{status: string, winner: string}>}
   */
  static async completeGame(roomId, winnerId, loserId) {
    const response = await fetch(`${API_BASE_URL}/game/complete`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ roomId, winnerId, loserId })
    });

    if (!response.ok) {
      throw new Error(`Complete game failed: ${response.statusText}`);
    }

    return await response.json();
  }
}

// Export for use in other files
if (typeof module !== 'undefined' && module.exports) {
  module.exports = BattleShipAPI;
}
