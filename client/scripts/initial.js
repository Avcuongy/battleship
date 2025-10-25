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

/**
 * Validate nickname input
 * Rules: 2-30 characters, alphanumeric + underscore + hyphen only
 */
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

/**
 * Update input visual feedback based on validation
 */
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

/**
 * Handle login form submission
 * Creates local player data and saves to Storage
 */
async function handleLogin(e) {
  e.preventDefault();

  const input = document.getElementById("nicknameInput");
  const button = document.getElementById("startButton");
  const nickname = input.value.trim();

  // Validate nickname
  const validation = validateNickname(nickname);
  if (!validation.valid) {
    alert(validation.message);
    input.focus();
    return;
  }

  // Disable button during processing
  button.disabled = true;
  button.textContent = "Đang xử lý...";

  try {
    // Generate player ID from backend (ensures uniqueness)
    const playerId = await API.generatePlayerId();
    
    if (!playerId) {
      throw new Error("Không thể tạo ID người chơi. Vui lòng thử lại!");
    }
    
    // Save to Storage utility (replaces direct localStorage calls)
    Storage.savePlayer(playerId, nickname);
    
    console.log("Player created:", { playerId, playerName: nickname });
    
    // Navigate to home page
    window.location.href = "./home.html";
  } catch (error) {
    console.error("Error creating player:", error);
    alert(`Có lỗi xảy ra: ${error.message}`);
    button.disabled = false;
    button.textContent = "BẮT ĐẦU";
  }
}

// ============================================================
// INITIALIZATION
// ============================================================

document.addEventListener("DOMContentLoaded", () => {
  // Initialize tutorial display
  updateTutorial();
  updatePagination();
  setupTutorialPagination();

  // Get DOM elements
  const button = document.getElementById("startButton");
  const input = document.getElementById("nicknameInput");

  // Initial button state
  button.disabled = true;
  button.style.opacity = "0.6";

  // Event listeners
  button.addEventListener("click", handleLogin);
  
  input.addEventListener("keypress", (e) => {
    if (e.key === "Enter") {
      button.click();
    }
  });
  
  input.addEventListener("input", (e) => {
    const nickname = e.target.value.trim();
    updateInputFeedback(input, nickname);
    
    const isValid = validateNickname(nickname).valid;
    button.disabled = !isValid;
    button.style.opacity = isValid ? "1" : "0.6";
  });

  // Auto-focus input
  input.focus();
});
