/**
 * WebSocket message protocol constants
 * Must match backend Network/Protocol.hs
 */

const Protocol = {
    // ============================================================================
    // Message Types (Client → Server)
    // ============================================================================

    CLIENT_MSG: {
        READY: 'ready',      // Player ready with fleet placement
        ATTACK: 'attack'     // Player attack
    },

    // ============================================================================
    // Message Types (Server → Client)
    // ============================================================================

    SERVER_MSG: {
        ATTACK_RESULT: 'attack_result',  // Attack result (broadcast to both)
        GAME_OVER: 'game_over',          // Game ended
        ERROR: 'error'                    // Error message
    },

    // ============================================================================
    // Result Types
    // ============================================================================

    RESULT: {
        MISS: 'miss',
        HIT: 'hit',
        SUNK: 'sunk'
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
        return JSON.stringify({
            type: this.CLIENT_MSG.READY,
            playerId: playerId,
            fleet: fleet
        });
    },

    /**
     * Build attack message
     * @param {string} playerId
     * @param {{posRow: number, posCol: number}} position
     * @returns {string} JSON string
     */
    buildAttackMessage(playerId, position) {
        return JSON.stringify({
            type: this.CLIENT_MSG.ATTACK,
            playerId: playerId,
            position: position
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
            return JSON.parse(data);
        } catch (error) {
            console.error('Failed to parse WebSocket message:', error);
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
    }
};

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
    module.exports = Protocol;
}
