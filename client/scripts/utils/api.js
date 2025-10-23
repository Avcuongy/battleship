/**
 * REST API wrapper for Battleship backend
 * Base URL: http://localhost:3000
 * All endpoints return JSON
 */

const API = {
    BASE_URL: 'http://localhost:3000',

    // ============================================================================
    // Core HTTP Methods
    // ============================================================================

    /**
     * Generic POST request
     * @param {string} endpoint - API endpoint (e.g., '/api/rooms/create')
     * @param {object} data - Request body
     * @returns {Promise<object|null>} Response JSON or null on error
     */
    async post(endpoint, data) {
        try {
            const response = await fetch(`${this.BASE_URL}${endpoint}`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify(data)
            });

            if (!response.ok) {
                console.error(`API Error: ${response.status} ${response.statusText}`);
                return null; // Silent ignore (per requirements Q26)
            }

            return await response.json();
        } catch (error) {
            console.error(`API Error (${endpoint}):`, error);
            return null; // Silent ignore
        }
    },

    /**
     * Generic GET request
     * @param {string} endpoint - API endpoint
     * @returns {Promise<object|null>} Response JSON or null on error
     */
    async get(endpoint) {
        try {
            const response = await fetch(`${this.BASE_URL}${endpoint}`, {
                method: 'GET',
                headers: {
                    'Content-Type': 'application/json'
                }
            });

            if (!response.ok) {
                console.error(`API Error: ${response.status} ${response.statusText}`);
                return null;
            }

            return await response.json();
        } catch (error) {
            console.error(`API Error (${endpoint}):`, error);
            return null;
        }
    },

    // ============================================================================
    // Room Endpoints (1vs1 mode)
    // ============================================================================

    /**
     * Create new room
     * @param {string} playerId - Player ID (6 chars)
     * @param {string} playerName - Player name
     * @returns {Promise<{crrRoomId: string, crrStatus: string}|null>}
     */
    async createRoom(playerId, playerName) {
        const response = await this.post('/api/rooms/create', {
            crPlayerId: playerId,
            crPlayerName: playerName
        });

        if (response && response.crrStatus === 'success') {
            console.log('Room created:', response.crrRoomId);
            return response;
        }

        console.error('Failed to create room');
        return null;
    },

    /**
     * Join existing room
     * @param {string} roomId - Room ID to join (6 chars case-sensitive)
     * @param {string} playerId - Player ID
     * @param {string} playerName - Player name
     * @returns {Promise<{jrrStatus: string, jrrMessage?: string}|null>}
     */
    async joinRoom(roomId, playerId, playerName) {
        const response = await this.post('/api/rooms/join', {
            jrRoomId: roomId,
            jrPlayerId: playerId,
            jrPlayerName: playerName
        });

        if (response && response.jrrStatus === 'success') {
            console.log('Joined room:', roomId);
            return response;
        }

        console.error('Failed to join room:', response?.jrrMessage);
        return null;
    },

    /**
     * Get room state
     * @param {string} roomId - Room ID
     * @returns {Promise<object|null>} Room state
     */
    async getRoomState(roomId) {
        return await this.get(`/api/rooms/${roomId}`);
    },

    // ============================================================================
    // AI Endpoints
    // ============================================================================

    /**
     * Start AI game session
     * @param {string} playerId - Player ID
     * @param {string} playerName - Player name
     * @param {Array} fleet - Array of 5 ships
     * @returns {Promise<{asrGameId: string, asrStatus: string, asrMessage?: string}|null>}
     * 
     * Fleet format:
     * [
     *   {
     *     shipType: "Destroyer",
     *     shipPosition: {posRow: 0, posCol: 0},
     *     shipOrientation: "Horizontal",
     *     shipHits: [false, false]
     *   },
     *   // ... 4 more ships
     * ]
     */
    async startAIGame(playerId, playerName, fleet) {
        const response = await this.post('/api/ai/start', {
            aiPlayerId: playerId,
            aiPlayerName: playerName,
            aiFleet: fleet
        });

        if (response && response.asrStatus === 'success') {
            console.log('AI game started:', response.asrGameId);
            return response;
        }

        console.error('Failed to start AI game:', response?.asrMessage);
        return null;
    },

    /**
     * Attack in AI game
     * @param {string} gameId - AI game session ID
     * @param {{posRow: number, posCol: number}} position - Attack position (0-9)
     * @returns {Promise<object|null>} Attack results
     * 
     * Response format:
     * {
     *   aarPlayerResult: {arPosition, arResult: "hit"|"miss"|"sunk", arShipType?},
     *   aarAiResult: {arPosition, arResult, arShipType?},
     *   aarGameOver: boolean,
     *   aarWinner: "player"|"ai"|null
     * }
     */
    async attackAI(gameId, position) {
        const response = await this.post('/api/ai/attack', {
            aaGameId: gameId,
            aaPosition: position
        });

        if (response) {
            console.log('Attack result:', response);
            return response;
        }

        console.error('Attack failed');
        return null;
    },

    // ============================================================================
    // Player Stats Endpoint
    // ============================================================================

    /**
     * Save player stats (called on beforeunload via sendBeacon)
     * Note: This is also called automatically by Storage.setupPersistence()
     * @param {string} playerId
     * @param {string} playerName
     * @param {number} gamesPlayed
     * @param {number} wins
     * @param {number} losses
     * @returns {Promise<object|null>}
     */
    async savePlayerStats(playerId, playerName, gamesPlayed, wins, losses) {
        return await this.post('/api/players/save', {
            spPlayerId: playerId,
            spPlayerName: playerName,
            spGamesPlayed: gamesPlayed,
            spWins: wins,
            spLosses: losses
        });
    },

    // ============================================================================
    // Utilities
    // ============================================================================

    /**
     * Check if backend is reachable
     * @returns {Promise<boolean>}
     */
    async healthCheck() {
        try {
            const response = await fetch(`${this.BASE_URL}/`, {
                method: 'GET'
            });
            return response.ok;
        } catch (error) {
            console.error('Backend not reachable:', error);
            return false;
        }
    }
};

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
    module.exports = API;
}
