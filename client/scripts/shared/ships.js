/**
 * Ship Placement Module
 * Click-based ship placement with hover preview
 */

const Ships = {
  // Ship types and sizes (must match backend)
  SHIP_TYPES: {
    'Carrier': 5,
    'Battleship': 4,
    'Cruiser': 3,
    'Submarine': 3,
    'Destroyer': 2
  },

  BOARD_SIZE: 10,

  // State
  fleet: [],
  currentShip: null,
  currentOrientation: 'Horizontal',
  boardElement: null,
  shipsContainer: null,
  isPlacementMode: false,

  // ============================================================================
  // Initialization
  // ============================================================================

  /**
   * Initialize click-based ship placement
   * @param {string} boardId - Board container ID
   * @param {string} shipsContainerId - Ships list container ID
   */
  init(boardId, shipsContainerId) {
    this.boardElement = document.getElementById(boardId);
    this.shipsContainer = document.getElementById(shipsContainerId);
    this.fleet = [];
    this.currentOrientation = 'Horizontal';
    this.isPlacementMode = false;

    if (!this.boardElement) {
      console.error('Board element not found:', boardId);
      return;
    }

    // Setup click-based placement
    this.setupShipSelection();
    this.setupBoardInteraction();
    this.setupRotation();

    console.log('Ship placement initialized (click mode)');
  },

  // ============================================================================
  // Ship Selection (Click Mode)
  // ============================================================================

  /**
   * Make ships selectable by click
   */
  setupShipSelection() {
    const shipItems = document.querySelectorAll('.ship-item');
    
    shipItems.forEach(shipItem => {
      shipItem.addEventListener('click', () => {
        // Ignore if already placed
        if (shipItem.classList.contains('placed')) {
          return;
        }

        const shipName = shipItem.dataset.name;
        const shipSize = parseInt(shipItem.dataset.size);
        
        // Deselect previous ship
        document.querySelectorAll('.ship-item').forEach(item => {
          item.classList.remove('selected');
        });

        // Select this ship
        shipItem.classList.add('selected');
        
        this.currentShip = {
          name: shipName,
          size: shipSize,
          element: shipItem
        };

        this.isPlacementMode = true;
        
        console.log('Ship selected:', shipName);
      });
    });
  },

  /**
   * Setup board interaction (hover preview + click to place)
   */
  setupBoardInteraction() {
    const cells = this.boardElement.querySelectorAll('.cell');
    
    cells.forEach(cell => {
      // Mouse enter - show preview
      cell.addEventListener('mouseenter', () => {
        if (this.isPlacementMode && this.currentShip) {
          const row = parseInt(cell.dataset.row);
          const col = parseInt(cell.dataset.col);
          this.showPlacementPreview(row, col);
        }
      });

      // Mouse leave - hide preview
      cell.addEventListener('mouseleave', () => {
        if (this.isPlacementMode) {
          this.hidePlacementPreview();
        }
      });

      // Left click - place ship
      cell.addEventListener('click', (e) => {
        if (this.isPlacementMode && this.currentShip) {
          const row = parseInt(cell.dataset.row);
          const col = parseInt(cell.dataset.col);
          this.placeShip(row, col);
        }
      });

      // Right click - rotate (on board)
      cell.addEventListener('contextmenu', (e) => {
        e.preventDefault();
        if (this.isPlacementMode) {
          this.rotate();
          // Re-show preview with new orientation
          const row = parseInt(cell.dataset.row);
          const col = parseInt(cell.dataset.col);
          this.showPlacementPreview(row, col);
        }
      });
    });
  },

  /**
   * Setup rotation (R key or right-click)
   */
  setupRotation() {
    // R key to rotate
    document.addEventListener('keydown', (e) => {
      if ((e.key === 'r' || e.key === 'R') && this.isPlacementMode) {
        this.rotate();
      }
    });

    // Global right-click prevention when placing
    document.addEventListener('contextmenu', (e) => {
      if (this.isPlacementMode) {
        e.preventDefault();
      }
    });
  },

  // ============================================================================
  // Ship Placement
  // ============================================================================

  /**
   * Place ship on board
   * @param {number} row - Starting row (0-9)
   * @param {number} col - Starting column (0-9)
   */
  placeShip(row, col) {
    if (!this.currentShip) return;

    const shipType = this.capitalizeShipName(this.currentShip.name);
    const ship = {
      shipType: shipType,
      shipPosition: { posRow: row, posCol: col },
      shipOrientation: this.currentOrientation,
      shipHits: new Array(this.currentShip.size).fill(false)
    };

    // Simple validation: only check bounds and overlap
    // Do NOT check if we have correct 5 ships (that's done on Ready button)
    const positions = this.getShipPositions(row, col, this.currentShip.size, this.currentOrientation);
    
    if (!this.isValidPlacement(positions)) {
      console.warn('Invalid placement: out of bounds or overlapping');
      this.showError('Vị trí không hợp lệ (vượt biên hoặc trùng tàu khác)');
      return;
    }

    // Add to fleet (no full validation yet)
    this.fleet.push(ship);
    
    console.log('Ship added to fleet:', ship);
    console.log('Current fleet size:', this.fleet.length);

    // Visual feedback - Display ship on board
    Board.placeShip(this.boardElement.id, ship);
    console.log('Ship displayed on board at:', ship.shipPosition);
    
    // Update ship item in list
    this.currentShip.element.classList.add('placed');
    this.currentShip.element.classList.remove('selected');

    console.log('Ship placement complete');
    
    // Exit placement mode
    this.isPlacementMode = false;
    this.currentShip = null;
    this.hidePlacementPreview();
    
    // Enable ready button if we have at least 5 ships
    // (full validation happens on button click)
    if (this.fleet.length >= 5) {
      const readyBtn = document.getElementById('readyBtn');
      if (readyBtn) {
        readyBtn.disabled = false;
      }
    }
  },

  /**
   * Show placement preview (highlight cells)
   * @param {number} row
   * @param {number} col
   */
  showPlacementPreview(row, col) {
    this.hidePlacementPreview(); // Clear previous preview

    if (!this.currentShip) return;

    const positions = this.getShipPositions(
      row, 
      col, 
      this.currentShip.size, 
      this.currentOrientation
    );

    // Check if valid placement
    const isValid = this.isValidPlacement(positions);

    positions.forEach(pos => {
      const cell = Board.getCell(this.boardElement.id, pos);
      if (cell) {
        cell.classList.add('preview');
        cell.classList.add(isValid ? 'valid' : 'invalid');
      }
    });
  },

  /**
   * Hide placement preview
   */
  hidePlacementPreview() {
    const cells = this.boardElement.querySelectorAll('.cell');
    cells.forEach(cell => {
      cell.classList.remove('preview', 'valid', 'invalid');
    });
  },

  /**
   * Check if placement is valid (not out of bounds, not overlapping)
   * @param {Array} positions
   * @returns {boolean}
   */
  isValidPlacement(positions) {
    // Check bounds
    for (const pos of positions) {
      if (pos.posRow < 0 || pos.posRow >= this.BOARD_SIZE ||
          pos.posCol < 0 || pos.posCol >= this.BOARD_SIZE) {
        return false;
      }
    }

    // Check overlap with existing ships
    for (const pos of positions) {
      const cell = Board.getCell(this.boardElement.id, pos);
      if (cell && cell.classList.contains('has-ship')) {
        return false;
      }
    }

    return true;
  },

  // ============================================================================
  // Ship Rotation
  // ============================================================================

  /**
   * Rotate current orientation (Horizontal ↔ Vertical)
   */
  rotate() {
    this.currentOrientation = 
      this.currentOrientation === 'Horizontal' ? 'Vertical' : 'Horizontal';
    
    console.log('Orientation changed to:', this.currentOrientation);
    
    // Update UI indicator (if exists)
    const orientationText = document.getElementById('orientationText');
    if (orientationText) {
      orientationText.textContent = this.currentOrientation;
    }
  },

  // ============================================================================
  // Fleet Management
  // ============================================================================

  /**
   * Get current fleet
   * @returns {Array} Fleet array (can be any number of ships)
   */
  getFleet() {
    return this.fleet;
  },

  /**
   * Reset all ships (remove from board)
   */
  reset() {
    // Clear board
    Board.clearShips(this.boardElement.id);
    
    // Reset ship items
    const shipItems = document.querySelectorAll('.ship-item');
    shipItems.forEach(item => {
      item.classList.remove('placed', 'selected');
    });

    // Reset state
    this.fleet = [];
    this.currentShip = null;
    this.currentOrientation = 'Horizontal';
    this.isPlacementMode = false;

    // Hide preview
    this.hidePlacementPreview();

    // Update orientation display
    const orientationText = document.getElementById('orientationText');
    if (orientationText) {
      orientationText.textContent = 'Horizontal';
    }

    // Disable ready button
    const readyBtn = document.getElementById('readyBtn');
    if (readyBtn) {
      readyBtn.disabled = true;
    }

    console.log('Ships reset');
  },

  // ============================================================================
  // Helper Functions
  // ============================================================================

  /**
   * Get positions for a ship at given location
   * @param {number} row
   * @param {number} col
   * @param {number} size
   * @param {string} orientation
   * @returns {Array<{posRow: number, posCol: number}>}
   */
  getShipPositions(row, col, size, orientation) {
    const positions = [];
    
    for (let i = 0; i < size; i++) {
      if (orientation === 'Horizontal') {
        positions.push({ posRow: row, posCol: col + i });
      } else {
        positions.push({ posRow: row + i, posCol: col });
      }
    }
    
    return positions;
  },

  /**
   * Capitalize ship name (carrier → Carrier)
   * @param {string} name
   * @returns {string}
   */
  capitalizeShipName(name) {
    return name.charAt(0).toUpperCase() + name.slice(1).toLowerCase();
  },

  /**
   * Show error message
   * @param {string} message
   */
  showError(message) {
    // Simple alert (can be replaced with nicer UI)
    console.error('Ship placement error:', message);
    
    // Optional: Show in UI
    const errorDiv = document.getElementById('placementError');
    if (errorDiv) {
      errorDiv.textContent = message;
      errorDiv.style.display = 'block';
      
      setTimeout(() => {
        errorDiv.style.display = 'none';
      }, 3000);
    }
  }
};

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
  module.exports = Ships;
}
