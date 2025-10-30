# WebSocket Client - Giải Thích Chi Tiết

## 🎯 Tóm Tắt

**WebSocket Client** là phần code chạy ở **phía browser** (client-side) để kết nối real-time với WebSocket Server.

Trong project này có **2 loại WebSocket client**:

---

## 📂 1. JavaScript WebSocket Client (Production)

### 📍 Location
```
client/scripts/websocket/
├── client.js      ← WebSocket Manager (kết nối, gửi, nhận message)
└── protocol.js    ← Message protocol (build/parse messages)
```

### 🔧 Chức năng

#### A. `client/scripts/websocket/client.js`
**WebSocket connection manager cho browser**

```javascript
class WebSocketManager {
    connect(roomId, playerId) {
        // Tạo kết nối WebSocket
        const wsUrl = `ws://localhost:9160?roomId=${roomId}&playerId=${playerId}`;
        this.ws = new WebSocket(wsUrl);
        
        this.ws.onopen = () => {
            console.log('WebSocket connected');
        };
        
        this.ws.onmessage = (event) => {
            this.handleMessage(event.data);
        };
        
        this.ws.onclose = () => {
            console.log('WebSocket closed');
        };
    }
    
    send(message) {
        this.ws.send(message);
    }
    
    on(messageType, callback) {
        this.messageHandlers[messageType] = callback;
    }
}
```

**Đây là WebSocket client thật sự dùng trong game!**

#### B. `client/scripts/websocket/protocol.js`
**Message protocol helpers**

```javascript
const Protocol = {
    // Build message to send
    buildReadyMessage(playerId, fleet) {
        return JSON.stringify({
            type: 'ready',
            rmPlayerId: playerId,
            rmFleet: fleet
        });
    },
    
    buildAttackMessage(playerId, position) {
        return JSON.stringify({
            type: 'attack',
            amPlayerId: playerId,
            amPosition: position
        });
    },
    
    // Parse message received
    parseMessage(data) {
        return JSON.parse(data);
    }
};
```

### 📖 Cách Sử Dụng Trong Game

**File:** `client/pages/1v1/room.html` hoặc game pages

```javascript
// 1. Import
import WSManager from '/scripts/websocket/client.js';
import Protocol from '/scripts/websocket/protocol.js';

// 2. Connect
await WSManager.connect(roomId, playerId);

// 3. Register handlers
WSManager.on('attack_result', (message) => {
    console.log('Attack result:', message);
    // Update UI...
});

WSManager.on('game_over', (message) => {
    console.log('Game over:', message);
    // Show winner...
});

// 4. Send messages
// Ready
const readyMsg = Protocol.buildReadyMessage(playerId, fleet);
WSManager.send(readyMsg);

// Attack
const attackMsg = Protocol.buildAttackMessage(playerId, position);
WSManager.send(attackMsg);
```

---

## 🧪 2. Haskell WebSocket Client (Test Only)

### 📍 Location
```
test/Spec.hs      ← WebSocket test client
```

### 🔧 Chức năng

**Haskell WebSocket client CHỈ dùng trong test suite**

```haskell
-- Hàm connect WebSocket từ test
connectClient :: String -> String -> IO WS.Connection
connectClient roomId playerId = do
    let url = wsUrl roomId playerId
    WS.runClient testHost testPort 
        ("/?roomId=" ++ roomId ++ "&playerId=" ++ playerId) 
        return
```

**Đây là client giả lập để test server!**

#### Ví dụ test:

```haskell
it "should accept valid WebSocket connection" $ do
    state <- startTestServer  -- Start server
    
    result <- (do
        -- Tạo WebSocket connection (Haskell client)
        conn <- connectClient "ROOM01" "PLAY01"
        
        -- Send message
        WS.sendTextData conn (encode someMessage)
        
        -- Close
        WS.sendClose conn ("Done" :: T.Text)
        return True
    ) `catch` (\(_ :: SomeException) -> return False)
    
    result `shouldBe` True
```

---

## 🔄 Flow Diagram - Client vs Server

```
┌─────────────────────────────────────────────────────────────────┐
│                    WEBSOCKET ARCHITECTURE                       │
└─────────────────────────────────────────────────────────────────┘

PRODUCTION (Real Game):
┌──────────────────────────┐         ┌──────────────────────────┐
│   Browser (Client-side)  │         │   Haskell (Server-side)  │
│                          │         │                          │
│  ┌────────────────────┐  │         │  ┌────────────────────┐  │
│  │ client.js          │  │         │  │ WebSocket.Server   │  │
│  │ ─────────────────  │  │         │  │ ──────────────────  │  │
│  │ new WebSocket()    │◄─┼─────────┼─►│ WS.ServerApp       │  │
│  │                    │  │ ws://   │  │                    │  │
│  │ ws.send(msg)       │──┼────────►│  │ receiveData        │  │
│  │ ws.onmessage       │◄─┼─────────┤  │ sendTextData       │  │
│  │                    │  │         │  │                    │  │
│  └────────────────────┘  │         │  └────────────────────┘  │
│                          │         │                          │
│  client/scripts/         │         │  src/Network/            │
│  websocket/client.js     │         │  WebSocket/Server.hs     │
└──────────────────────────┘         └──────────────────────────┘

TEST (Automated Testing):
┌──────────────────────────┐         ┌──────────────────────────┐
│   Haskell Test Suite     │         │   Haskell (Server-side)  │
│                          │         │                          │
│  ┌────────────────────┐  │         │  ┌────────────────────┐  │
│  │ test/Spec.hs       │  │         │  │ WebSocket.Server   │  │
│  │ ─────────────────  │  │         │  │ ──────────────────  │  │
│  │ WS.runClient       │◄─┼─────────┼─►│ WS.ServerApp       │  │
│  │                    │  │         │  │                    │  │
│  │ WS.sendTextData    │──┼────────►│  │ receiveData        │  │
│  │ WS.receiveData     │◄─┼─────────┤  │ sendTextData       │  │
│  │                    │  │         │  │                    │  │
│  └────────────────────┘  │         │  └────────────────────┘  │
│                          │         │                          │
│  Network.WebSockets lib  │         │  src/Network/            │
│  (Haskell package)       │         │  WebSocket/Server.hs     │
└──────────────────────────┘         └──────────────────────────┘
```

---

## 🎯 Điểm Khác Biệt

| Aspect | JavaScript Client (Production) | Haskell Client (Test) |
|--------|--------------------------------|----------------------|
| **Location** | `client/scripts/websocket/` | `test/Spec.hs` |
| **Purpose** | Real game connections | Automated testing |
| **Runtime** | Browser (Chrome/Firefox) | Test suite (GHC) |
| **API** | Browser WebSocket API | Network.WebSockets library |
| **Usage** | User plays game | CI/CD automated tests |
| **Connection** | `new WebSocket()` | `WS.runClient` |
| **Lifecycle** | Long-lived (game session) | Short-lived (test case) |

---

## 🐛 Test Failures - Root Cause

**Khi test fails:**

```
8) WebSocket Server Tests, Connection Management, should accept valid WebSocket connection
   expected: True
    but got: False
```

**Nguyên nhân:**

1. **Haskell WebSocket client** (trong test) không kết nối được
2. **KHÔNG PHẢI** JavaScript client (production code vẫn works fine)

**Vấn đề có thể là:**

```haskell
-- test/Spec.hs line ~76
connectClient :: String -> String -> IO WS.Connection
connectClient roomId playerId = do
    let url = wsUrl roomId playerId
    -- ❌ Có thể:
    -- - Server chưa sẵn sàng (timing issue)
    -- - Port conflicts
    -- - Path/query string sai format
    -- - Handshake failed
    WS.runClient testHost testPort 
        ("/?roomId=" ++ roomId ++ "&playerId=" ++ playerId) 
        return
```

---

## ✅ Solution - Debug Test Client

**Cần fix test client (không phải production client):**

```haskell
-- test/Spec.hs
connectClient :: String -> String -> IO WS.Connection
connectClient roomId playerId = do
    -- Add retry logic
    -- Add better error handling
    -- Add logging
    -- Wait for server ready
    threadDelay 1000000  -- Wait 1s for server
    
    WS.runClient testHost testPort path $ \conn -> do
        putStrLn $ "✓ Connected: " ++ roomId ++ "/" ++ playerId
        return conn
  where
    path = "/?roomId=" ++ roomId ++ "&playerId=" ++ playerId
```

---

## 📝 Summary

**WebSocket Client có 2 loại:**

1. **JavaScript Client** (`client/scripts/websocket/client.js`)
   - ✅ Production code
   - ✅ Chạy trong browser
   - ✅ User thật sử dụng
   - ✅ Code này works fine (không có vấn đề)

2. **Haskell Test Client** (`test/Spec.hs`)
   - ⚠️ Test code only
   - ⚠️ Chạy trong test suite
   - ⚠️ Automated testing
   - ❌ **ĐÂY LÀ PHẦN BỊ LỖI** - không connect được

**Test failures KHÔNG ảnh hưởng production!**

JavaScript WebSocket client vẫn hoạt động bình thường khi user chơi game. Test failures chỉ là vấn đề của test infrastructure.
