/**
 * LocalStorage wrapper for Battleship game
 * Handles player data, room info, and game state persistence
 */

const Storage = {
    // Keys
    KEYS: {
        PLAYER_ID: 'battleship_playerId',
        PLAYER_NAME: 'battleship_playerName',
        ROOM_ID: 'battleship_roomId',
        GAME_ID: 'battleship_gameId',
        PLAYER_STATS: 'battleship_stats',
        FLEET: 'battleship_fleet'
    },

    // ============================================================================
    // Player Data
    // ============================================================================

    /**
     * Save player ID (6 chars a-zA-Z)
     * @param {string} playerId
     */
    savePlayerId(playerId) {
        try {
            localStorage.setItem(this.KEYS.PLAYER_ID, playerId);
            console.log('Saved playerId:', playerId);
        } catch (error) {
            console.error('Error saving playerId:', error);
        }
    },

    /**
     * Get player ID
     * @returns {string|null}
     */
    getPlayerId() {
        return localStorage.getItem(this.KEYS.PLAYER_ID);
    },

    /**
     * Save player name
     * @param {string} name
     */
    savePlayerName(name) {
        try {
            localStorage.setItem(this.KEYS.PLAYER_NAME, name);
            console.log('Saved playerName:', name);
        } catch (error) {
            console.error('Error saving playerName:', error);
        }
    },

    /**
     * Get player name
     * @returns {string|null}
     */
    getPlayerName() {
        return localStorage.getItem(this.KEYS.PLAYER_NAME);
    },

    /**
     * Save both player ID and name
     * @param {string} playerId
     * @param {string} playerName
     */
    savePlayer(playerId, playerName) {
        this.savePlayerId(playerId);
        this.savePlayerName(playerName);
    },

    /**
     * Get player data (both ID and name)
     * @returns {{playerId: string|null, playerName: string|null}}
     */
    getPlayer() {
        return {
            playerId: this.getPlayerId(),
            playerName: this.getPlayerName()
        };
    },

    // ============================================================================
    // Room Data (1vs1 mode)
    // ============================================================================

    /**
     * Save room ID (6 chars a-zA-Z case-sensitive)
     * @param {string} roomId
     */
    saveRoomId(roomId) {
        try {
            localStorage.setItem(this.KEYS.ROOM_ID, roomId);
            console.log('Saved roomId:', roomId);
        } catch (error) {
            console.error('Error saving roomId:', error);
        }
    },

    /**
     * Get room ID
     * @returns {string|null}
     */
    getRoomId() {
        return localStorage.getItem(this.KEYS.ROOM_ID);
    },

    /**
     * Clear room ID (when game ends or disconnect)
     */
    clearRoomId() {
        localStorage.removeItem(this.KEYS.ROOM_ID);
        console.log('Cleared roomId');
    },

    // ============================================================================
    // Game Data (AI mode)
    // ============================================================================

    /**
     * Save AI game session ID
     * @param {string} gameId
     */
    saveGameId(gameId) {
        try {
            sessionStorage.setItem(this.KEYS.GAME_ID, gameId);
            console.log('Saved gameId:', gameId);
        } catch (error) {
            console.error('Error saving gameId:', error);
        }
    },

    /**
     * Get AI game session ID
     * @returns {string|null}
     */
    getGameId() {
        return sessionStorage.getItem(this.KEYS.GAME_ID);
    },

    /**
     * Clear game ID
     */
    clearGameId() {
        sessionStorage.removeItem(this.KEYS.GAME_ID);
        console.log('Cleared gameId');
    },

    // ============================================================================
    // Player Stats
    // ============================================================================

    /**
     * Save player stats (games played, wins, losses)
     * @param {{gamesPlayed: number, wins: number, losses: number}} stats
     */
    saveStats(stats) {
        try {
            localStorage.setItem(this.KEYS.PLAYER_STATS, JSON.stringify(stats));
            console.log('Saved stats:', stats);
        } catch (error) {
            console.error('Error saving stats:', error);
        }
    },

    /**
     * Get player stats
     * @returns {{gamesPlayed: number, wins: number, losses: number}}
     */
    getStats() {
        try {
            const stats = localStorage.getItem(this.KEYS.PLAYER_STATS);
            return stats ? JSON.parse(stats) : { gamesPlayed: 0, wins: 0, losses: 0 };
        } catch (error) {
            console.error('Error getting stats:', error);
            return { gamesPlayed: 0, wins: 0, losses: 0 };
        }
    },

    /**
     * Update stats after game end
     * @param {boolean} won - true if player won
     */
    updateStats(won) {
        const stats = this.getStats();
        stats.gamesPlayed += 1;
        if (won) {
            stats.wins += 1;
        } else {
            stats.losses += 1;
        }
        this.saveStats(stats);
    },

    // ============================================================================
    // Persistence (Save on page unload)
    // ============================================================================

    /**
     * Setup beforeunload handler to save stats to backend
     * Uses navigator.sendBeacon for non-blocking request
     */
    setupPersistence() {
        window.addEventListener('beforeunload', () => {
            const player = this.getPlayer();
            const stats = this.getStats();

            if (player.playerId && player.playerName) {
                const data = JSON.stringify({
                    spPlayerId: player.playerId,
                    spPlayerName: player.playerName,
                    spGamesPlayed: stats.gamesPlayed,
                    spWins: stats.wins,
                    spLosses: stats.losses
                });

                // Non-blocking request (fires even if page closes)
                navigator.sendBeacon('/api/players/save', data);
                console.log('Stats saved via sendBeacon');
            }
        });
    },

    // ============================================================================
    // Utilities
    // ============================================================================

    /**
     * Save fleet data (array of ships)
     * @param {Array} fleet - Array of ship objects
     */
    saveFleet(fleet) {
        try {
            localStorage.setItem(this.KEYS.FLEET, JSON.stringify(fleet));
            console.log('Saved fleet:', fleet);
        } catch (error) {
            console.error('Error saving fleet:', error);
        }
    },

    /**
     * Get fleet data
     * @returns {Array|null}
     */
    getFleet() {
        try {
            const data = localStorage.getItem(this.KEYS.FLEET);
            return data ? JSON.parse(data) : null;
        } catch (error) {
            console.error('Error getting fleet:', error);
            return null;
        }
    },

    /**
     * Clear fleet data
     */
    clearFleet() {
        localStorage.removeItem(this.KEYS.FLEET);
        console.log('Cleared fleet');
    },

    /**
     * Clear all game data (logout/reset)
     */
    clearAll() {
        Object.values(this.KEYS).forEach(key => {
            localStorage.removeItem(key);
        });
        sessionStorage.clear();
        console.log('Cleared all storage');
    },

    /**
     * Check if player is initialized (has ID and name)
     * @returns {boolean}
     */
    isPlayerInitialized() {
        const player = this.getPlayer();
        return !!(player.playerId && player.playerName);
    },

    /**
     * Debug: Print all stored data
     */
    debug() {
        console.log('=== Storage Debug ===');
        console.log('Player:', this.getPlayer());
        console.log('Room ID:', this.getRoomId());
        console.log('Game ID:', this.getGameId());
        console.log('Stats:', this.getStats());
        console.log('====================');
    }
};

// Initialize persistence on load
Storage.setupPersistence();

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
    module.exports = Storage;
}
