// AI Loading Page
// Displays player info and allows starting the game

document.addEventListener('DOMContentLoaded', () => {
    // Check if user is logged in
    if (!SessionManager.isLoggedIn()) {
        console.log('No session found, redirecting to initial...');
        window.location.href = '../initial.html';
        return;
    }
    
    // Check if room was created
    const gameState = SessionManager.getGameState();
    if (!gameState || !gameState.roomId || gameState.gameMode !== 'AI') {
        console.log('No AI room found, redirecting to home...');
        window.location.href = '../home.html';
        return;
    }
    
    // Load and display player data
    const session = SessionManager.getSession();
    const player = session.player;
    
    document.getElementById('player1Name').textContent = player.name;
    document.getElementById('player1Id').textContent = `ID: ${player.id}`;
    
    // Enable start button immediately (AI is always ready)
    const startButton = document.getElementById('startButton');
    startButton.disabled = false;
    
    // Handle start button click
    startButton.addEventListener('click', async () => {
        startButton.disabled = true;
        startButton.textContent = 'ĐANG BẮT ĐẦU...';
        
        try {
            // Set player as ready
            console.log('Setting player ready...');
            const response = await BattleShipAPI.setReady(
                gameState.roomId,
                player.id,
                true
            );
            
            console.log('Ready response:', response);
            
            if (response.canStart) {
                // Navigate to setup page
                startButton.textContent = 'BẮT ĐẦU!';
                setTimeout(() => {
                    window.location.href = './setup.html';
                }, 300);
            } else {
                alert('Chưa thể bắt đầu. Vui lòng thử lại!');
                startButton.disabled = false;
                startButton.textContent = 'BẮT ĐẦU';
            }
            
        } catch (error) {
            console.error('Failed to start game:', error);
            alert('Không thể bắt đầu trò chơi. Vui lòng thử lại!');
            startButton.disabled = false;
            startButton.textContent = 'BẮT ĐẦU';
        }
    });
    
    console.log('AI Loading page loaded, roomId:', gameState.roomId);
});
