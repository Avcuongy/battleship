/**
 * WebSocket connection manager for real-time 1vs1 mode
 * Connects to: ws://localhost:9160
 */

class WebSocketManager {
    constructor() {
        this.ws = null;
        this.roomId = null;
        this.playerId = null;
        this.messageHandlers = {};
        this.reconnectAttempts = 0;
        this.maxReconnectAttempts = 0; // No reconnect (per requirements Q19)
    }

    // ============================================================================
    // Connection Management
    // ============================================================================

    /**
     * Connect to WebSocket server
     * @param {string} roomId - Room ID (6 chars case-sensitive)
     * @param {string} playerId - Player ID (6 chars)
     * @returns {Promise<boolean>} Success status
     */
    connect(roomId, playerId) {
        return new Promise((resolve, reject) => {
            this.roomId = roomId;
            this.playerId = playerId;

            const wsUrl = `ws://localhost:9160?roomId=${roomId}&playerId=${playerId}`;
            console.log('Connecting to WebSocket:', wsUrl);

            try {
                this.ws = new WebSocket(wsUrl);

                // Connection opened
                this.ws.onopen = () => {
                    console.log('WebSocket connected');
                    this.reconnectAttempts = 0;
                    resolve(true);
                };

                // Message received
                this.ws.onmessage = (event) => {
                    this.handleMessage(event.data);
                };

                // Connection closed
                this.ws.onclose = (event) => {
                    console.log('WebSocket closed:', event.code, event.reason);
                    this.handleDisconnect();
                };

                // Error occurred
                this.ws.onerror = (error) => {
                    console.error('WebSocket error:', error);
                    reject(error);
                };

            } catch (error) {
                console.error('Failed to create WebSocket:', error);
                reject(error);
            }
        });
    },

    /**
     * Disconnect WebSocket
     */
    disconnect() {
        if (this.ws) {
            console.log('Disconnecting WebSocket');
            this.ws.close();
            this.ws = null;
        }
    },

    /**
     * Check if connected
     * @returns {boolean}
     */
    isConnected() {
        return this.ws && this.ws.readyState === WebSocket.OPEN;
    },

    // ============================================================================
    // Message Handling
    // ============================================================================

    /**
     * Send message to server
     * @param {string} message - JSON string (use Protocol.buildXXXMessage)
     * @returns {boolean} Success status
     */
    send(message) {
        if (!this.isConnected()) {
            console.error('WebSocket not connected');
            return false;
        }

        try {
            this.ws.send(message);
            console.log('Sent WebSocket message:', message);
            return true;
        } catch (error) {
            console.error('Failed to send message:', error);
            return false;
        }
    },

    /**
     * Handle incoming message from server
     * @param {string} data - Raw message data (JSON string)
     */
    handleMessage(data) {
        console.log('Received WebSocket message:', data);

        const message = Protocol.parseMessage(data);
        if (!message) {
            return; // Silent ignore invalid messages (per requirements)
        }

        // Call registered handler for this message type
        const handler = this.messageHandlers[message.type];
        if (handler) {
            handler(message);
        } else {
            console.warn('No handler for message type:', message.type);
        }
    },

    /**
     * Register message handler
     * @param {string} messageType - Message type (e.g., Protocol.SERVER_MSG.ATTACK_RESULT)
     * @param {function} callback - Handler function (receives message object)
     */
    on(messageType, callback) {
        this.messageHandlers[messageType] = callback;
        console.log('Registered handler for:', messageType);
    },

    /**
     * Unregister message handler
     * @param {string} messageType
     */
    off(messageType) {
        delete this.messageHandlers[messageType];
    },

    // ============================================================================
    // Disconnect Handling (Per Requirements Q19)
    // ============================================================================

    /**
     * Handle disconnect (no reconnect, opponent auto-wins)
     */
    handleDisconnect() {
        console.log('Handling disconnect...');
        
        // Per requirements: Disconnect = game over, no reconnect
        // Redirect to home or show game over
        if (this.messageHandlers['disconnect']) {
            this.messageHandlers['disconnect']();
        } else {
            // Default behavior: alert and redirect
            alert('Connection lost. Game ended.');
            window.location.href = '/pages/home.html';
        }
    },

    // ============================================================================
    // Game Actions (Convenience Methods)
    // ============================================================================

    /**
     * Send ready message with fleet
     * @param {Array} fleet - Array of 5 ships
     * @returns {boolean} Success status
     */
    sendReady(fleet) {
        const message = Protocol.buildReadyMessage(this.playerId, fleet);
        return this.send(message);
    },

    /**
     * Send attack message
     * @param {{posRow: number, posCol: number}} position
     * @returns {boolean} Success status
     */
    sendAttack(position) {
        const message = Protocol.buildAttackMessage(this.playerId, position);
        return this.send(message);
    }
}

// Create singleton instance
const WSManager = new WebSocketManager();

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
    module.exports = WSManager;
}
