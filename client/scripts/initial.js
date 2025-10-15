const tutorialSteps = [
  { number: "1", text: "Mỗi người chơi có 2 lưới 10×10" },
  { number: "2", text: "Đặt 5 tàu trên bảng: 1 tàu 2 ô, 2 tàu 3 ô, 1 tàu 4 ô, 1 tàu 5 ô" },
  { number: "3", text: "Lần lượt gọi tọa độ để tấn công" },
  { number: "4", text: "Trúng thì tiếp tục đánh, trượt (Miss) thì chuyển lượt" },
  { number: "5", text: "Mỗi lượt có thời gian giới hạn" },
  { number: "6", text: "Người đầu tiên đánh chìm hết 5 tàu của đối thủ sẽ thắng" },
];

let currentStep = 0;

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
  dots.forEach((dot, index) => {
    dot.classList.toggle("active", index === currentStep);
  });
}

function setupTutorialPagination() {
  const dots = document.querySelectorAll(".dot");
  dots.forEach((dot, index) => {
    dot.addEventListener("click", (e) => {
      e.preventDefault();
      currentStep = index;
      updateTutorial();
      updatePagination();
    });
  });
}

async function loginPlayer(nickname) {
  const response = await fetch("http://localhost:3000/api/login", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ loginName: nickname }),
  });

  if (!response.ok) {
    throw new Error(`HTTP ${response.status}`);
  }

  const data = await response.json();
  if (!data.success) {
    throw new Error(data.message || "Login failed");
  }

  // Map API response to SessionManager format
  return {
    playerId: data.playerId,
    name: data.playerName || nickname,
    stats: {} // Default empty stats
  };
}

function validateNickname(nickname) {
  if (!nickname || nickname.length < 2) {
    return { valid: false, message: "Biệt danh phải có ít nhất 2 ký tự" };
  }
  if (nickname.length > 30) {
    return { valid: false, message: "Biệt danh tối đa 30 ký tự" };
  }
  if (!/^[a-zA-Z0-9_-]+$/.test(nickname)) {
    return { valid: false, message: "Chỉ được chứa chữ cái, số, _ và -" };
  }
  return { valid: true };
}

function updateInputFeedback(input, nickname) {
  input.style.borderColor = "";
  if (nickname && !/^[a-zA-Z0-9_-]*$/.test(nickname)) {
    input.style.borderColor = "#d32f2f";
  } else if (nickname.length > 30) {
    input.style.borderColor = "#d32f2f";
  } else if (nickname.length >= 2) {
    input.style.borderColor = "#4caf50";
  }
}

async function handleLogin(e) {
  e.preventDefault();

  const input = document.getElementById("nicknameInput");
  const button = document.getElementById("startButton");
  const nickname = input.value.trim();

  const validation = validateNickname(nickname);
  if (!validation.valid) {
    alert(validation.message);
    input.focus();
    return;
  }

  button.disabled = true;
  button.textContent = "Đang đăng nhập...";

  try {
    const player = await loginPlayer(nickname);
    
    // Use SessionManager to save session properly
    SessionManager.saveSession(player);
    
    console.log("Login success:", player);
    window.location.href = "./home.html";
  } catch (error) {
    console.error("Login error:", error);
    alert(`Không thể đăng nhập!\n\nLỗi: ${error.message}\n\nKiểm tra server tại http://localhost:3000`);
    button.disabled = false;
    button.textContent = "BẮT ĐẦU";
  }
}

// ============================================================
// INITIALIZATION
// ============================================================

document.addEventListener("DOMContentLoaded", () => {
  updateTutorial();
  updatePagination();
  setupTutorialPagination();

  const button = document.getElementById("startButton");
  const input = document.getElementById("nicknameInput");

  button.disabled = true;
  button.style.opacity = "0.6";

  button.addEventListener("click", handleLogin);
  input.addEventListener("keypress", (e) => e.key === "Enter" && button.click());
  input.addEventListener("input", (e) => {
    const nickname = e.target.value.trim();
    updateInputFeedback(input, nickname);
    const isValid = validateNickname(nickname).valid;
    button.disabled = !isValid;
    button.style.opacity = isValid ? "1" : "0.6";
  });

  input.focus();
});
