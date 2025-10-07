// Avatar characters array
const avatars = [
  "/assets/images/daden.jpg",
  "/assets/images/captain.jpg",
  "/assets/images/female.jpg",
];

let currentAvatarIndex = 0;

// Tutorial steps
const tutorialSteps = [
  {
    number: "1",
    text: "Chọn chế độ chơi: 1 người (với AI) hoặc 2 người",
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

// Check if images load properly
function checkImageLoad() {
  const img = document.querySelector("#avatar img");
  if (img) {
    img.onerror = function () {
      console.error("Failed to load image:", this.src);
      // Fallback to emoji if image fails
      document.getElementById("avatar").innerHTML = "👨🏿‍💼";
    };
    img.onload = function () {
      console.log("Image loaded successfully:", this.src);
    };
  }
}

// Initialize on page load
window.addEventListener("load", function () {
  checkImageLoad();
});

// Avatar refresh functionality
document
  .getElementById("refreshAvatar")
  .addEventListener("click", function () {
    currentAvatarIndex = (currentAvatarIndex + 1) % avatars.length;
    const newImg = `<img src="${avatars[currentAvatarIndex]}" alt="avatar">`;
    document.getElementById("avatar").innerHTML = newImg;

    // Check if new image loads
    setTimeout(checkImageLoad, 100);

    // Add animation effect
    const avatar = document.getElementById("avatar");
    avatar.style.transform = "scale(0.8)";
    setTimeout(() => {
      avatar.style.transform = "scale(1)";
    }, 150);
  });

// Tutorial pagination
const dots = document.querySelectorAll(".dot");
const tutorialContent = document.getElementById("tutorialContent");

dots.forEach((dot, index) => {
  dot.addEventListener("click", function () {
    currentStep = index;
    updateTutorial();
    updatePagination();
  });
});

function updateTutorial() {
  const step = tutorialSteps[currentStep];
  tutorialContent.innerHTML = `
    <div class="step-number">${step.number}</div>
    <div class="step-description">${step.text}</div>
  `;
}

function updatePagination() {
  dots.forEach((dot, index) => {
    dot.classList.toggle("active", index === currentStep);
  });
}

// Start button functionality
document
  .getElementById("startButton")
  .addEventListener("click", function () {
    const nickname = document
      .getElementById("nicknameInput")
      .value.trim();
    if (nickname) {
      alert(`Chào mừng ${nickname}! Trò chơi sẽ bắt đầu...`);
      // Here you would typically navigate to the game screen
    } else {
      alert("Vui lòng nhập biệt danh!");
      document.getElementById("nicknameInput").focus();
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