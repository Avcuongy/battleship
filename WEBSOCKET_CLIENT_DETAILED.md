# 🔍 WebSocket Client - So Sánh Trực Quan

## 📍 Vị Trí Trong Code

```
BattleShip/
├── client/scripts/websocket/
│   └── client.js          ← ✅ JAVASCRIPT CLIENT (Production)
└── test/
    └── Spec.hs            ← ⚠️ HASKELL CLIENT (Test only)
```

---

## 🎯 1. JavaScript WebSocket Client (Production)

### 📂 File: `client/scripts/websocket/client.js`

```javascript
/**
 * ✅ ĐÂY LÀ CLIENT THẬT - CHẠY TRONG BROWSER
 * User chơi game → Dùng client này
 */

class WebSocketManager {
    constructor() {
        this.ws = null;              // ← WebSocket instance
        this.roomId = null;
        this.playerId = null;
        this.messageHandlers = {};
    }

    // KẾT NỐI WEBSOCKET
    connect(roomId, playerId) {
        return new Promise((resolve, reject) => {
            // ★ TẠO WEBSOCKET CONNECTION
            const wsUrl = `ws://localhost:9160?roomId=${roomId}&playerId=${playerId}`;
            this.ws = new WebSocket(wsUrl);  // ← Browser WebSocket API
            
            // ★ XỬ LÝ EVENTS
            this.ws.onopen = () => {
                console.log('✓ WebSocket connected');
                resolve(true);
            };
            
            this.ws.onmessage = (event) => {
                this.handleMessage(event.data);  // ← Nhận message từ server
            };
            
            this.ws.onclose = (event) => {
                console.log('WebSocket closed');
                this.handleDisconnect();
            };
            
            this.ws.onerror = (error) => {
                console.error('❌ WebSocket error:', error);
                reject(error);
            };
        });
    }
    
    // GỬI MESSAGE
    send(message) {
        if (this.isConnected()) {
            this.ws.send(message);  // ← Gửi đến server
        }
    }
    
    // ĐĂNG KÝ HANDLER
    on(messageType, callback) {
        this.messageHandlers[messageType] = callback;
    }
}

// ★ SINGLETON INSTANCE
const WSManager = new WebSocketManager();

// ★ EXPORT RA NGOÀI
if (typeof module !== 'undefined' && module.exports) {
    module.exports = WSManager;
}
```

### 🎮 Cách Sử Dụng (Game Pages)

```javascript
// File: client/pages/1v1/room.html hoặc game.html

// 1. Import
import WSManager from '/scripts/websocket/client.js';

// 2. Connect khi vào room
await WSManager.connect(roomId, playerId);

// 3. Đăng ký handlers
WSManager.on('attack_result', (msg) => {
    console.log('Kết quả tấn công:', msg);
    updateBoard(msg);
});

WSManager.on('game_over', (msg) => {
    console.log('Game kết thúc, winner:', msg.gomWinner);
    showGameOverModal(msg);
});

// 4. Gửi ready
const readyMsg = Protocol.buildReadyMessage(playerId, fleet);
WSManager.send(readyMsg);

// 5. Gửi attack
const attackMsg = Protocol.buildAttackMessage(playerId, position);
WSManager.send(attackMsg);
```

---

## 🧪 2. Haskell WebSocket Client (Test Only)

### 📂 File: `test/Spec.hs`

```haskell
{- 
 ⚠️ ĐÂY LÀ TEST CLIENT - CHỈ DÙNG TRONG TEST SUITE
 Automated testing → Dùng client này
-}

import qualified Network.WebSockets as WS

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Start test WebSocket server
startTestServer :: IO WebSocketState
startTestServer = do
    roomMgr <- RoomMgr.newRoomManager
    playerMgr <- PlayerMgr.newPlayerManager
    wsConnections <- newTVarIO Map.empty
    
    let state = WebSocketState
            { wsRoomManager = roomMgr
            , wsPlayerManager = playerMgr
            , wsConnections = wsConnections
            }
    
    -- ★ START SERVER IN BACKGROUND
    _ <- forkIO $ WSServer.startWebSocketServer state
    threadDelay 500000  -- Wait 0.5s for server to start
    
    return state

-- | Connect WebSocket client (TEST CLIENT)
-- ★★★ ĐÂY LÀ PHẦN BỊ LỖI ★★★
connectClient :: String -> String -> IO WS.Connection
connectClient roomId playerId = do
    let url = wsUrl roomId playerId
    -- ★ KẾT NỐI BẰNG HASKELL WEBSOCKET LIBRARY
    WS.runClient testHost testPort 
        ("/?roomId=" ++ roomId ++ "&playerId=" ++ playerId) 
        return
    -- ❌ Connection failed → Test failed

-- ============================================================================
-- Main Test Suite
-- ============================================================================

main :: IO ()
main = hspec $ do
    describe "WebSocket Server Tests" $ do
        
        describe "Connection Management" $ do
            it "should accept valid WebSocket connection" $ do
                state <- startTestServer  -- Start server
                
                result <- (do
                    -- ★ TẠO TEST CONNECTION
                    conn <- connectClient "ROOM01" "PLAY01"  -- ← Lỗi ở đây!
                    
                    WS.sendClose conn ("Test done" :: T.Text)
                    return True
                    ) `catch` (\(_ :: SomeException) -> return False)
                
                result `shouldBe` True
                -- Expected: True
                -- But got: False  ← ❌ Test failed
```

### 🧪 Cách Sử Dụng (Test Suite)

```bash
# Chạy test
stack test

# Test sẽ:
# 1. Start WebSocket server (Haskell)
# 2. Create test client (Haskell WS.runClient)
# 3. Try to connect
# 4. ❌ Connection failed → Test failed
```

---

## 🔄 Side-by-Side Comparison

| Aspect | JavaScript Client | Haskell Test Client |
|--------|------------------|---------------------|
| **File** | `client/scripts/websocket/client.js` | `test/Spec.hs` |
| **Line** | Lines 1-204 | Lines 73-79 |
| **Purpose** | Production game client | Automated testing |
| **Who uses** | Players (humans) | CI/CD (machines) |
| **Runtime** | Browser (Chrome/Firefox) | Test suite (GHC) |
| **API** | `new WebSocket(url)` | `WS.runClient host port path` |
| **Status** | ✅ **WORKS FINE** | ❌ **FAILING** |
| **Impact** | User plays game normally | Tests fail in CI/CD |

---

## 🎯 Visual Flow

```
┌─────────────────────────────────────────────────────────────────┐
│              WEBSOCKET CLIENT COMPARISON                        │
└─────────────────────────────────────────────────────────────────┘

PRODUCTION (JavaScript Client):
┌──────────────────────────────────────────────────────────┐
│ Browser (User opens game page)                           │
│                                                           │
│  1. Load: client/scripts/websocket/client.js             │
│  2. User clicks "Join Room"                              │
│  3. Call: WSManager.connect(roomId, playerId)            │
│  4. Create: new WebSocket('ws://localhost:9160?...')     │
│  5. Server accepts connection                            │
│  6. ✅ SUCCESS - User can play                           │
│                                                           │
│  Status: ✅ WORKING                                      │
└──────────────────────────────────────────────────────────┘

TEST (Haskell Client):
┌──────────────────────────────────────────────────────────┐
│ Test Suite (stack test command)                          │
│                                                           │
│  1. Load: test/Spec.hs                                   │
│  2. Test runs: "should accept connection"                │
│  3. Call: startTestServer (background thread)            │
│  4. Call: connectClient "ROOM01" "PLAY01"                │
│  5. Try: WS.runClient testHost testPort path             │
│  6. ❌ FAILED - Connection refused/timeout               │
│                                                           │
│  Status: ❌ FAILING (8 out of 12 tests failed)          │
└──────────────────────────────────────────────────────────┘
```

---

## 🐛 Debug Information

### ❌ Test Failures (8 failed)

```
Failures:

  1) WebSocket Server Tests, Connection Management, 
     should accept valid WebSocket connection
       expected: True
        but got: False
     
     ↑ This means: connectClient() in test/Spec.hs failed to connect
```

### ✅ JavaScript Client (Still Working)

```javascript
// Users can still play game normally!
// client.js connects successfully to ws://localhost:9160
// No impact on production
```

---

## 📝 Kết Luận

### WebSocket Client Là Gì?

**2 phần riêng biệt:**

1. **`client/scripts/websocket/client.js`** 
   - ✅ Production WebSocket client
   - Chạy trong browser
   - Users dùng để chơi game
   - **KHÔNG CÓ VẤN ĐỀ**

2. **`test/Spec.hs` (connectClient function)**
   - ⚠️ Test WebSocket client
   - Chạy trong test suite
   - CI/CD dùng để test server
   - **ĐÂY LÀ PHẦN BỊ LỖI**

### Vấn Đề Nằm Ở Đâu?

**Test client (Haskell) không connect được:**

```haskell
-- test/Spec.hs line 73-79
connectClient :: String -> String -> IO WS.Connection
connectClient roomId playerId = do
    let url = wsUrl roomId playerId
    WS.runClient testHost testPort      -- ← ❌ Failed here
        ("/?roomId=" ++ roomId ++ "&playerId=" ++ playerId) 
        return
```

**Có thể do:**
- Timing issue (server chưa ready)
- Path format không đúng
- Port conflict
- WebSocket handshake failed

### ✅ Production Client Vẫn OK

```javascript
// client/scripts/websocket/client.js
this.ws = new WebSocket(wsUrl);  // ← ✅ Still works!
```

**User vẫn chơi game bình thường!**
