/**
 * WebSocket message protocol constants
 * Must match backend Network/Protocol.hs
 */

const Protocol = {
  // ============================================================================
  // Message Types (Client → Server)
  // ============================================================================

  CLIENT_MSG: {
    READY: "ready", // Player ready with fleet placement
    ATTACK: "attack", // Player attack
    START: "start", // Host signals to start setup
  },

  // ============================================================================
  // Message Types (Server → Client)
  // ============================================================================

  SERVER_MSG: {
    ATTACK_RESULT: "attack_result", // Attack result (broadcast to both)
    GAME_OVER: "game_over", // Game ended
    GAME_START: "game_start", // Navigate both players to setup phase
    PLAYER_READY: "player_ready", // Player ready status update
    ERROR: "error", // Error message
  },

  // ============================================================================
  // Result Types
  // ============================================================================

  RESULT: {
    MISS: "miss",
    HIT: "hit",
    SUNK: "sunk",
  },

  // ============================================================================
  // Message Builders (Client → Server)
  // ============================================================================

  /**
   * Build ready message
   * @param {string} playerId
   * @param {Array} fleet - Array of 5 ships
   * @returns {string} JSON string
   */
  buildReadyMessage(playerId, fleet) {
    // Match Haskell Aeson default encoding for ClientMessage
    return JSON.stringify({
      tag: "ReadyMsg",
      contents: {
        rmPlayerId: playerId,
        rmFleet: fleet,
      },
    });
  },

  /**
   * Build attack message
   * @param {string} playerId
   * @param {{posRow: number, posCol: number}} position
   * @returns {string} JSON string
   */
  buildAttackMessage(playerId, position) {
    // Match Haskell Aeson default encoding for ClientMessage
    return JSON.stringify({
      tag: "AttackMsg",
      contents: {
        amPlayerId: playerId,
        amPosition: position,
      },
    });
  },

  /**
   * Build start message (host only)
   * @param {string} playerId
   * @returns {string} JSON string
   */
  buildStartMessage(playerId) {
    // Match Haskell Aeson default encoding for ClientMessage
    return JSON.stringify({
      tag: "StartMsg",
      contents: {
        smPlayerId: playerId,
      },
    });
  },

  // ============================================================================
  // Message Parsers (Server → Client)
  // ============================================================================

  /**
   * Parse incoming WebSocket message
   * @param {string} data - JSON string from server
   * @returns {object|null} Parsed message or null on error
   */
  parseMessage(data) {
    try {
      const raw = JSON.parse(data);
      if (!raw || typeof raw !== "object") return null;

      // If server already sends { type: '...' } shape, keep as-is
      if (Object.prototype.hasOwnProperty.call(raw, "type")) {
        return raw;
      }

      // Normalize Haskell Aeson default sum encoding: { tag: 'Ctor', contents: { ... } }
      const tag = raw.tag || raw.Tag || raw.constructor;
      const contents = raw.contents || raw.Contents || raw;
      switch (tag) {
        case "GameStartMsg":
          return {
            type: this.SERVER_MSG.GAME_START,
            roomId: contents.gsmRoomId,
          };
        case "PlayerReadyMsg":
          return {
            type: this.SERVER_MSG.PLAYER_READY,
            playerId: contents.prmPlayerId,
            ready: contents.prmReady,
          };
        case "AttackResultMsg":
          return {
            type: this.SERVER_MSG.ATTACK_RESULT,
            attacker: contents.armAttacker,
            position: contents.armPosition,
            result: contents.armResult,
            shipType: contents.armShipType,
            nextTurn: contents.armNextTurn,
          };
        case "GameOverMsg":
          return {
            type: this.SERVER_MSG.GAME_OVER,
            winner: contents.gomWinner,
            winnerName: contents.gomWinnerName,
            reason: contents.gomReason,
          };
        case "ErrorMsg":
          return {
            type: this.SERVER_MSG.ERROR,
            error: contents.emError,
            details: contents.emDetails,
          };
        default:
          return raw;
      }
    } catch (error) {
      console.error("Failed to parse WebSocket message:", error);
      return null; // Silent ignore (per requirements)
    }
  },

  /**
   * Check message type
   * @param {object} message
   * @param {string} expectedType
   * @returns {boolean}
   */
  isMessageType(message, expectedType) {
    return message && message.type === expectedType;
  },
};

// Export for use in other modules
if (typeof module !== "undefined" && module.exports) {
  module.exports = Protocol;
}
