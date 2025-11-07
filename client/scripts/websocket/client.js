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
      this._opened = false;
      // Clear previous keepalive if any
      if (this._keepAliveTimer) {
        clearInterval(this._keepAliveTimer);
        this._keepAliveTimer = null;
      }

      // Allow full URL override via localStorage (useful for ngrok)
      const wsOverride = (typeof window !== "undefined" && window.localStorage)
        ? window.localStorage.getItem("battleship-ws-url")
        : null;

      const wsProto =
        typeof window !== "undefined" &&
        window.location &&
        window.location.protocol === "https:"
          ? "wss"
          : "ws";
      const wsHost =
        typeof window !== "undefined" &&
        window.location &&
        window.location.hostname
          ? window.location.hostname
          : "localhost";
      // Include explicit '/' before query to ensure server receives '/?roomId=...'
      let wsUrl;
      if (wsOverride && typeof wsOverride === "string" && wsOverride.trim().length > 0) {
        const base = wsOverride.trim();
        const sep = base.includes("?") ? "&" : "?";
        wsUrl = `${base}${sep}roomId=${roomId}&playerId=${playerId}`;
      } else {
        wsUrl = `${wsProto}://${wsHost}:9160/?roomId=${roomId}&playerId=${playerId}`;
      }
      console.log("Connecting to WebSocket:", wsUrl);

      try {
        this.ws = new WebSocket(wsUrl);
        let settled = false;

        // Connection opened
        this.ws.onopen = () => {
          console.log("WebSocket connected");
          this.reconnectAttempts = 0;
          this._opened = true;
          // Send a tiny keepalive right after open to avoid aggressive proxies closing idle sockets
          try {
            this.ws.send('{"type":"ping"}');
          } catch (_) {}
          // And schedule periodic keepalive every 20s
          this._keepAliveTimer = setInterval(() => {
            if (this.isConnected()) {
              try {
                this.ws.send('{"type":"ping"}');
              } catch (_) {}
            }
          }, 20000);
          if (!settled) {
            settled = true;
            resolve(true);
          }
        };

        // Message received
        this.ws.onmessage = (event) => {
          this.handleMessage(event.data);
        };

        // Connection closed
        this.ws.onclose = (event) => {
          console.log("WebSocket closed:", event.code, event.reason);
          if (this._keepAliveTimer) {
            clearInterval(this._keepAliveTimer);
            this._keepAliveTimer = null;
          }
          if (this._opened) {
            // Real disconnect after being connected
            this.handleDisconnect(event);
          } else {
            // Closed before open – treat as connect failure
            console.warn("WebSocket closed before opening");
            if (!settled) {
              settled = true;
              reject(new Error("WebSocket closed before open"));
            }
          }
        };

        // Error occurred
        this.ws.onerror = (error) => {
          console.error("WebSocket error:", error);
          if (!settled) {
            settled = true;
            reject(error);
          }
        };
      } catch (error) {
        console.error("Failed to create WebSocket:", error);
        reject(error);
      }
    });
  }

  /**
   * Disconnect WebSocket
   */
  disconnect() {
    if (this.ws) {
      console.log("Disconnecting WebSocket");
      this.ws.close();
      this.ws = null;
    }
  }

  /**
   * Check if connected
   * @returns {boolean}
   */
  isConnected() {
    return this.ws && this.ws.readyState === WebSocket.OPEN;
  }

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
      console.error("WebSocket not connected");
      return false;
    }

    try {
      this.ws.send(message);
      console.log("Sent WebSocket message:", message);
      return true;
    } catch (error) {
      console.error("Failed to send message:", error);
      return false;
    }
  }

  /**
   * Handle incoming message from server
   * @param {string} data - Raw message data (JSON string)
   */
  handleMessage(data) {
    console.log("Received WebSocket message:", data);

    const message = Protocol.parseMessage(data);
    if (!message) {
      return; // Silent ignore invalid messages (per requirements)
    }

    // Call registered handler for this message type
    const handler = this.messageHandlers[message.type];
    if (handler) {
      handler(message);
    } else {
      console.warn("No handler for message type:", message.type);
    }
  }

  /**
   * Register message handler
   * @param {string} messageType - Message type (e.g., Protocol.SERVER_MSG.ATTACK_RESULT)
   * @param {function} callback - Handler function (receives message object)
   */
  on(messageType, callback) {
    this.messageHandlers[messageType] = callback;
    console.log("Registered handler for:", messageType);
  }

  /**
   * Unregister message handler
   * @param {string} messageType
   */
  off(messageType) {
    delete this.messageHandlers[messageType];
  }

  // ============================================================================
  // Disconnect Handling (Per Requirements Q19)
  // ============================================================================

  /**
   * Handle disconnect (no reconnect, opponent auto-wins)
   */
  handleDisconnect(event) {
    const code =
      event && typeof event.code === "number" ? event.code : "(unknown)";
    const reason = event && event.reason ? event.reason : "";
    console.log("Handling disconnect...", code, reason);

    // Per requirements: Disconnect = game over, no reconnect
    // Redirect to home or show game over
    if (this.messageHandlers["disconnect"]) {
      this.messageHandlers["disconnect"]({ code, reason });
    } else {
      // Default behavior: alert and redirect
      const extra = reason ? ` (reason: ${reason})` : "";
      alert(`Connection lost. Game ended. Code: ${code}${extra}`);
      window.location.href = "/pages/home.html";
    }
  }

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
  }

  /**
   * Send attack message
   * @param {{posRow: number, posCol: number}} position
   * @returns {boolean} Success status
   */
  sendAttack(position) {
    const message = Protocol.buildAttackMessage(this.playerId, position);
    return this.send(message);
  }

  /**
   * Send start message (host only)
   * @returns {boolean}
   */
  sendStart() {
    const message = Protocol.buildStartMessage(this.playerId);
    return this.send(message);
  }
}

// Create singleton instance
const WSManager = new WebSocketManager();

// Export for use in other modules
if (typeof module !== "undefined" && module.exports) {
  module.exports = WSManager;
}
