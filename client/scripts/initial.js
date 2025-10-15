// IndexedDB helpers to persist File System Access handles across pages
function openFSDB() {
  return new Promise((resolve, reject) => {
    const req = indexedDB.open("battleship-fs", 1);
    req.onupgradeneeded = () => {
      const db = req.result;
      if (!db.objectStoreNames.contains("handles")) {
        db.createObjectStore("handles");
      }
    };
    req.onsuccess = () => resolve(req.result);
    req.onerror = () => reject(req.error);
  });
}

async function fsdbPut(key, value) {
  const db = await openFSDB();
  return new Promise((resolve, reject) => {
    const tx = db.transaction("handles", "readwrite");
    tx.objectStore("handles").put(value, key);
    tx.oncomplete = () => resolve();
    tx.onerror = () => reject(tx.error);
  });
}

async function fsdbGet(key) {
  const db = await openFSDB();
  return new Promise((resolve, reject) => {
    const tx = db.transaction("handles", "readonly");
    const req = tx.objectStore("handles").get(key);
    req.onsuccess = () => resolve(req.result);
    req.onerror = () => reject(req.error);
  });
}

async function fsdbDelete(key) {
  const db = await openFSDB();
  return new Promise((resolve, reject) => {
    const tx = db.transaction("handles", "readwrite");
    tx.objectStore("handles").delete(key);
    tx.oncomplete = () => resolve();
    tx.onerror = () => reject(tx.error);
  });
}

// Silent storage helpers (no prompts/no folder selection)
async function getStoredDirHandle() {
  try {
    return await fsdbGet("playersDirHandle");
  } catch (e) {
    return null;
  }
}

async function ensureRWPermission(handle) {
  try {
    if (!handle) return false;
    if (handle.queryPermission) {
      const cur = await handle.queryPermission({ mode: "readwrite" });
      if (cur === "granted") return true;
      // Do not requestPermission to avoid prompts
      return false;
    }
    return true;
  } catch (e) {
    return false;
  }
}

async function saveToOPFS(fileName, contents) {
  try {
    if (!navigator.storage?.getDirectory) return false;
    const root = await navigator.storage.getDirectory();
    const dataDir = await root.getDirectoryHandle("data", {
      create: true,
    });
    const playersDir = await dataDir.getDirectoryHandle("players", {
      create: true,
    });
    const fileHandle = await playersDir.getFileHandle(fileName, {
      create: true,
    });
    const writable = await fileHandle.createWritable();
    await writable.write(contents);
    await writable.close();

    // Lưu thông tin về file và directory để các trang khác có thể truy cập
    await fsdbPut("opfsPlayersDir", playersDir).catch(() => {});
    localStorage.setItem("battleship-player-file", fileName);
    localStorage.setItem("battleship-storage-backend", "opfs");
    console.log("OPFS save successful:", fileName);
    return true;
  } catch (e) {
    console.warn("OPFS save failed:", e);
    return false;
  }
}

// Helper: resolve a (possibly relative) path to an absolute URL
function toAbsolutePath(p) {
  try {
    if (!p) return "";
    // Normalize Windows-style backslashes
    const normalized = String(p).replace(/\\/g, "/");
    return new URL(normalized, document.baseURI).href;
  } catch (e) {
    return p;
  }
}

// Player session management functions
function generatePlayerId() {
  // Generate 6 random characters (letters and numbers only)
  const chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  let result = "";
  for (let i = 0; i < 6; i++) {
    result += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return result; // Chỉ trả về 6 ký tự, không thêm prefix
}

// File System Management - Ephemeral file storage with File System Access API
let tempDirHandle = null; // keep in-memory for this page lifetime
let tempFileName = null;

async function savePlayerToFile(playerData) {
  try {
    const fileName = `${playerData.id}.json`;
    const playerJson = JSON.stringify(playerData, null, 2);

    // Ưu tiên lưu vào OPFS trước để đảm bảo dữ liệu được giữ lại
    // mà không cần tương tác người dùng
    let saved = false;

    // Thử lưu vào OPFS (Origin Private File System)
    const opfsSaved = await saveToOPFS(fileName, playerJson);
    if (opfsSaved) {
      console.log("Đã lưu thông tin người chơi vào OPFS:", fileName);
      tempFileName = fileName;
      await fsdbPut("playerFileName", fileName).catch(() => {});
      localStorage.setItem("battleship-player-file", fileName);
      saved = true;
    }

    // Nếu OPFS không khả dụng hoặc thất bại, thử lưu qua File System Access API
    if (!saved && "showDirectoryPicker" in window) {
      const dirHandle = await getStoredDirHandle();
      if (await ensureRWPermission(dirHandle)) {
        try {
          const fileHandle = await dirHandle.getFileHandle(fileName, {
            create: true,
          });
          const writable = await fileHandle.createWritable();
          await writable.write(playerJson);
          await writable.close();
          tempDirHandle = dirHandle;
          tempFileName = fileName;
          await fsdbPut("playerFileName", fileName).catch(() => {});
          await fsdbPut("playersDirHandle", dirHandle).catch(() => {});
          localStorage.setItem("battleship-player-file", fileName);
          localStorage.setItem("battleship-storage-backend", "fs");
          saved = true;
          console.log("Đã lưu thông tin người chơi vào FS:", fileName);
        } catch (e) {
          console.warn("Write via stored dir handle failed:", e);
        }
      }
    }

    // Nếu không thể lưu file, báo lỗi nhưng vẫn tiếp tục với localStorage
    if (!saved) {
      console.warn("Không thể lưu vào OPFS hoặc FS; tiếp tục với localStorage");
      return false;
    }

    return true;
  } catch (error) {
    console.error("Error saving player data:", error);
    return false;
  }
}

// Fallback download disabled to meet "no prompts/notifications"
async function fallbackDownload(playerData, fileName, playerJson) {
  return false;
}

// Auto cleanup file when page unloads (if we have directory handle)
async function cleanupSavedFiles() {
  try {
    // Try in-memory first
    let dirHandle = tempDirHandle;
    let fileName = tempFileName;

    // Then try from IndexedDB
    if (!dirHandle) {
      dirHandle = await fsdbGet("playersDirHandle");
    }
    if (!fileName) {
      fileName =
        (await fsdbGet("playerFileName")) ||
        localStorage.getItem("battleship-player-file");
    }

    if (
      dirHandle &&
      fileName &&
      localStorage.getItem("battleship-storage-backend") === "fs"
    ) {
      try {
        await dirHandle.removeEntry(fileName);
        console.log("Đã xóa file:", fileName);
      } catch (err) {
        console.warn("Không thể xóa file (có thể đã bị xóa trước đó):", err);
      }
    }

    // OPFS cleanup if used
    if (localStorage.getItem("battleship-storage-backend") === "opfs") {
      try {
        if (navigator.storage?.getDirectory) {
          const root = await navigator.storage.getDirectory();
          const dataDir = await root.getDirectoryHandle("data");
          const playersDir = await dataDir.getDirectoryHandle("players");
          if (fileName) {
            await playersDir.removeEntry(fileName);
            console.log("Đã xóa file (OPFS):", fileName);
          }
        }
      } catch (e) {
        console.warn("Không thể xóa file OPFS:", e);
      }
    }

    // Clear markers
    await fsdbDelete("playerFileName").catch(() => {});
    await fsdbDelete("playersDirHandle").catch(() => {});
    localStorage.removeItem("battleship-player-file");
    localStorage.removeItem("battleship-storage-backend");
  } catch (e) {
    console.warn("cleanupSavedFiles error:", e);
  }
}

async function savePlayerSession(nickname) {
  // Call backend API to create player
  try {
    const response = await fetch("http://localhost:3000/api/login", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ loginName: nickname }),
    });

    if (!response.ok) {
      throw new Error(`API error: ${response.status}`);
    }

    const apiData = await response.json();
    console.log("API response:", apiData);

    if (!apiData.success) {
      throw new Error(apiData.message || "Login failed");
    }

    // Use server-generated player ID - only 2 fields: id, name
    const playerData = {
      id: apiData.playerId,
      name: apiData.playerName || nickname,
    };

    // Save to localStorage
    localStorage.setItem(
      "battleship-player-session",
      JSON.stringify(playerData)
    );

    // Save player data to JSON file (optional, for local persistence)
    await savePlayerToFile(playerData);

    console.log("Player session saved:", playerData);

    return playerData;
  } catch (error) {
    console.error("Failed to create player via API:", error);
    throw error; // Re-throw to handle in caller
  }
}

function loadPlayerSession() {
  const saved = localStorage.getItem("battleship-player-session");
  return saved ? JSON.parse(saved) : null;
}

// Tutorial steps
const tutorialSteps = [
  {
    number: "1",
    text: "Mỗi người chơi có 2 lưới 10×10",
  },
  {
    number: "2",
    text: "Đặt 5 tàu trên bảng: 1 tàu 2 ô, 2 tàu 3 ô, 1 tàu 4 ô, 1 tàu 5 ô",
  },
  {
    number: "3",
    text: "Lần lượt gọi tọa độ để tấn công",
  },
  {
    number: "4",
    text: "Trúng thì tiếp tục đánh, trượt (Miss) thì chuyển lượt",
  },
  {
    number: "5",
    text: "Mỗi lượt có thời gian giới hạn",
  },
  {
    number: "6",
    text: "Người đầu tiên đánh chìm hết 5 tàu của đối thủ sẽ thắng",
  },
];

let currentStep = 0;

// Initialize on page load
window.addEventListener("load", function () {
  // Initialize tutorial content
  updateTutorial();
  updatePagination();

  // Clear any existing session data on fresh page load
  // This ensures a completely fresh start every time
  console.log("Page loaded - clearing any existing session data");
});

// Tutorial pagination - setup when DOM is ready
function setupTutorialPagination() {
  const dots = document.querySelectorAll(".dot");
  const tutorialContent = document.getElementById("tutorialContent");

  if (!dots.length || !tutorialContent) {
    console.error("Tutorial elements not found!");
    return;
  }

  dots.forEach((dot, index) => {
    dot.addEventListener("click", function (e) {
      e.preventDefault();
      currentStep = parseInt(dot.getAttribute("data-step") || index);
      updateTutorial();
      updatePagination();
      console.log("Tutorial step changed to:", currentStep + 1);
    });
  });
}

function updateTutorial() {
  const tutorialContent = document.getElementById("tutorialContent");
  if (!tutorialContent) return;

  const step = tutorialSteps[currentStep] || tutorialSteps[0];
  tutorialContent.innerHTML = `
            <div class="step-number">${step.number}</div>
            <div class="step-description">${step.text}</div>
        `;
}

function updatePagination() {
  const dots = document.querySelectorAll(".dot");
  if (!dots.length) return;

  dots.forEach((dot, index) => {
    if (index === currentStep) {
      dot.classList.add("active");
    } else {
      dot.classList.remove("active");
    }
  });
}

// Setup tutorial pagination when DOM is loaded
document.addEventListener("DOMContentLoaded", setupTutorialPagination);

// Start button functionality with session save
document
  .getElementById("startButton")
  .addEventListener("click", async function (e) {
    e.preventDefault(); // Prevent any default form submission

    console.log("Start button clicked");
    const nickname = document.getElementById("nicknameInput").value.trim();

    // Validation
    if (!nickname) {
      alert("Vui lòng nhập biệt danh!");
      document.getElementById("nicknameInput").focus();
      return;
    }

    if (nickname.length < 2) {
      alert("Biệt danh phải có ít nhất 2 ký tự.");
      document.getElementById("nicknameInput").focus();
      return;
    }

    if (nickname.length > 30) {
      alert("Biệt danh không được vượt quá 30 ký tự.");
      document.getElementById("nicknameInput").focus();
      return;
    }

    // Validate nickname pattern
    const nicknamePattern = /^[a-zA-Z0-9_-]+$/;
    if (!nicknamePattern.test(nickname)) {
      alert(
        "Biệt danh chỉ được chứa chữ cái, số, dấu gạch dưới _ và dấu gạch ngang -"
      );
      document.getElementById("nicknameInput").focus();
      return;
    }

    // Disable the button to prevent multiple clicks
    const startButton = document.getElementById("startButton");
    startButton.disabled = true;

    try {
      // Save player session and data
      const playerData = await savePlayerSession(nickname);

      // Player created with ID
      console.log(`Player created: ${playerData.id}`);

      // Mark that we're navigating within the app (don't delete file/session on this unload)
      sessionStorage.setItem("bs-keep-file", "1");

      // Navigate to home page after a short delay to allow file save to complete
      try {
        // Save player session and data
        const playerData = await savePlayerSession(nickname);

        console.log(`Player created: ${playerData.id}`);

        // localStorage.setItem() trong savePlayerSession() đã xong
        const savedSession = localStorage.getItem("battleship-player-session");
        if (!savedSession) {
          throw new Error("Session không được lưu");
        }

        console.log("Session saved successfully, redirecting...");

        // Redirect ngay - KHÔNG cần setTimeout
        window.location.href = "../pages/home.html";
      } catch (error) {
        console.error("Lỗi khi đăng nhập:", error);
        alert("Không thể đăng nhập: " + error.message);
        startButton.disabled = false;
        startButton.textContent = "BẮT ĐẦU";
      }
    } catch (error) {
      console.error("Lỗi khi đăng nhập:", error);
      let errorMsg = "Không thể kết nối đến server. ";
      if (error.message) {
        errorMsg += error.message;
      }
      errorMsg += "\n\nVui lòng kiểm tra:\n";
      errorMsg += "1. Server đang chạy tại http://localhost:3000\n";
      errorMsg += "2. Kết nối mạng\n";
      errorMsg += "3. CORS được cấu hình đúng";
      alert(errorMsg);

      // Re-enable the button
      startButton.disabled = false;
      startButton.textContent = "BẮT ĐẦU";
    }
  });

// Input enter key support
document
  .getElementById("nicknameInput")
  .addEventListener("keypress", function (e) {
    if (e.key === "Enter") {
      document.getElementById("startButton").click();
    }
  });

// Real-time nickname validation feedback
document
  .getElementById("nicknameInput")
  .addEventListener("input", function (e) {
    const nickname = e.target.value.trim();
    const nicknamePattern = /^[a-zA-Z0-9_-]*$/;

    // Reset border color
    e.target.style.borderColor = "";

    if (nickname.length > 0 && !nicknamePattern.test(nickname)) {
      e.target.style.borderColor = "#d32f2f";
      e.target.title = "Chỉ được chứa chữ cái, số, _ và -";
    } else if (nickname.length > 30) {
      e.target.style.borderColor = "#d32f2f";
      e.target.title = "Tối đa 30 ký tự";
    } else if (nickname.length >= 2) {
      e.target.style.borderColor = "#4caf50";
      e.target.title = "Biệt danh hợp lệ";
    }
  });

// Update start button state based on input
document
  .getElementById("nicknameInput")
  .addEventListener("input", function (e) {
    const nickname = e.target.value.trim();
    const startButton = document.getElementById("startButton");
    const isValid =
      nickname.length >= 2 &&
      nickname.length <= 30 &&
      /^[a-zA-Z0-9_-]+$/.test(nickname);

    if (isValid) {
      startButton.style.opacity = "1";
      startButton.style.cursor = "pointer";
      startButton.disabled = false;
    } else {
      startButton.style.opacity = "0.6";
      startButton.style.cursor = "not-allowed";
      startButton.disabled = true;
    }
  });

// Initialize button state
window.addEventListener("DOMContentLoaded", function () {
  const startButton = document.getElementById("startButton");
  startButton.style.opacity = "0.6";
  startButton.style.cursor = "not-allowed";
  startButton.disabled = true;

  // Check if nickname field has value and update button state accordingly
  const nicknameInput = document.getElementById("nicknameInput");
  if (nicknameInput.value.trim()) {
    nicknameInput.dispatchEvent(new Event("input"));
  }

  // Set up tutorial
  updateTutorial();
  updatePagination();
});
