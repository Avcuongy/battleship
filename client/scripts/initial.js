// Initialize API client and Session Manager
const api = new BattleShipAPI();
const session = new SessionManager();

// DOM Elements
let nicknameInput;
let startButton;
let loadingMessage;
let errorMessage;

// Initialize page
document.addEventListener('DOMContentLoaded', () => {
    nicknameInput = document.getElementById('nicknameInput');
    startButton = document.getElementById('startButton');
    loadingMessage = document.getElementById('loadingMessage');
    
    // Create error message element if it doesn't exist
    errorMessage = document.getElementById('errorMessage');
    if (!errorMessage) {
        errorMessage = document.createElement('div');
        errorMessage.id = 'errorMessage';
        errorMessage.className = 'error-message';
        errorMessage.style.display = 'none';
        errorMessage.style.color = '#ff4444';
        errorMessage.style.marginTop = '10px';
        errorMessage.style.padding = '10px';
        errorMessage.style.borderRadius = '5px';
        errorMessage.style.backgroundColor = 'rgba(255, 68, 68, 0.1)';
        nicknameInput.parentElement.appendChild(errorMessage);
    }
    
    // Check if user already has a valid session
    if (session.isSessionValid()) {
        const sessionData = session.getSession();
        console.log('Valid session found:', sessionData);
        redirectToHome();
        return;
    }
    
    // Set up event listeners
    startButton.addEventListener('click', handleStart);
    nicknameInput.addEventListener('keypress', (e) => {
        if (e.key === 'Enter') {
            handleStart();
        }
    });
    
    // Auto-focus nickname input
    nicknameInput.focus();
    
    console.log('Initial page loaded');
});

// Handle start button click
async function handleStart() {
    const nickname = nicknameInput.value.trim();
    
    // Validate nickname
    if (!nickname) {
        showError('Vui lòng nhập tên của bạn!');
        nicknameInput.focus();
        return;
    }
    
    if (nickname.length < 2) {
        showError('Tên phải có ít nhất 2 ký tự!');
        nicknameInput.focus();
        return;
    }
    
    if (nickname.length > 20) {
        showError('Tên không được quá 20 ký tự!');
        nicknameInput.focus();
        return;
    }
    
    // Disable input and button during login
    nicknameInput.disabled = true;
    startButton.disabled = true;
    hideError();
    showLoading('Đang đăng nhập...');
    
    try {
        // Call login API
        console.log('Attempting login with nickname:', nickname);
        const response = await api.login(nickname);
        
        console.log('Login successful:', response);
        
        // Save session data
        session.saveSession(response.playerId, response.playerName);
        
        // Show success message briefly
        showLoading('Đăng nhập thành công! Đang chuyển hướng...');
        
        // Redirect to home page after brief delay
        setTimeout(() => {
            redirectToHome();
        }, 800);
        
    } catch (error) {
        console.error('Login failed:', error);
        showError('Không thể đăng nhập. Vui lòng thử lại!');
        
        // Re-enable input and button
        nicknameInput.disabled = false;
        startButton.disabled = false;
        hideLoading();
        nicknameInput.focus();
    }
}

// Show loading message
function showLoading(message) {
    if (loadingMessage) {
        loadingMessage.textContent = message;
        loadingMessage.style.display = 'block';
    }
}

// Hide loading message
function hideLoading() {
    if (loadingMessage) {
        loadingMessage.style.display = 'none';
    }
}

// Show error message
function showError(message) {
    if (errorMessage) {
        errorMessage.textContent = message;
        errorMessage.style.display = 'block';
    }
}

// Hide error message
function hideError() {
    if (errorMessage) {
        errorMessage.style.display = 'none';
    }
}

// Redirect to home page
function redirectToHome() {
    window.location.href = './home.html';
}