document.addEventListener('DOMContentLoaded', () => {
    // Check if user is logged in
    if (!SessionManager.isLoggedIn()) {
        console.log('No session found, redirecting to initial...');
        window.location.href = './initial.html';
        return;
    }
    
    // Load and display player data
    const session = SessionManager.getSession();
    const player = session.player;
    
    document.getElementById('playerName').textContent = player.name;
    document.getElementById('playerId').textContent = `ID: ${player.id}`;
    
    // Set up mode selection buttons
    const singlePlayerBtn = document.getElementById('singlePlayerBtn');
    const twoPlayerBtn = document.getElementById('twoPlayerBtn');
    
    // Single player (AI mode) - Active
    singlePlayerBtn.addEventListener('click', async () => {
        singlePlayerBtn.style.opacity = '0.7';
        singlePlayerBtn.style.pointerEvents = 'none';
        
        try {
            // Create AI room
            console.log('Creating AI room...');
            const roomData = await BattleShipAPI.createRoom(player.id, 'AI');
            
            console.log('Room created:', roomData);
            
            // Save room data to game state
            SessionManager.updateGameState({
                roomId: roomData.roomId,
                gameMode: 'AI',
                playerId: player.id
            });
            
            // Navigate to AI loading page
            window.location.href = './ai/loading.html';
            
        } catch (error) {
            console.error('Failed to create AI room:', error);
            alert('Không thể tạo phòng chơi. Vui lòng thử lại!');
            singlePlayerBtn.style.opacity = '1';
            singlePlayerBtn.style.pointerEvents = 'auto';
        }
    });
    
    // Two player (1vs1 mode) - Disabled (do not handle)
    twoPlayerBtn.addEventListener('click', () => {
        alert('Chế độ 2 người chưa được hỗ trợ!');
    });
    
    console.log('Home page loaded for player:', player.name);
});