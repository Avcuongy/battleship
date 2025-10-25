/**
 * Board Rendering and Interaction Module
 * Handles 10x10 grid rendering, ship display, attack markers
 */

const Board = {
  BOARD_SIZE: 10,
  
  // Cell states
  CELL_STATE: {
    EMPTY: 'empty',
    SHIP: 'ship',
    HIT: 'hit',
    MISS: 'miss',
    SUNK: 'sunk'
  },

  // ============================================================================
  // Board Rendering
  // ============================================================================

  /**
   * Render a 10x10 board
   * @param {string} containerId - DOM element ID for board container
   * @param {boolean} isPlayerBoard - If true, shows ships. If false, hides ships (enemy board)
   * @returns {HTMLElement} The board element
   */
  render(containerId, isPlayerBoard = true) {
    const container = document.getElementById(containerId);
    if (!container) {
      console.error(`Board container not found: ${containerId}`);
      return null;
    }

    // Clear existing content
    container.innerHTML = '';
    container.classList.add('game-board');
    
    // Store board type as data attribute
    container.dataset.boardType = isPlayerBoard ? 'player' : 'enemy';

    // Create 10x10 grid
    for (let row = 0; row < this.BOARD_SIZE; row++) {
      for (let col = 0; col < this.BOARD_SIZE; col++) {
        const cell = document.createElement('div');
        cell.classList.add('cell');
        cell.dataset.row = row;
        cell.dataset.col = col;
        cell.dataset.state = this.CELL_STATE.EMPTY;
        
        container.appendChild(cell);
      }
    }

    console.log(`Board rendered: ${containerId} (${isPlayerBoard ? 'player' : 'enemy'})`);
    return container;
  },

  /**
   * Get cell element by position
   * @param {string} containerId - Board container ID
   * @param {{posRow: number, posCol: number}} position
   * @returns {HTMLElement|null}
   */
  getCell(containerId, position) {
    const container = document.getElementById(containerId);
    if (!container) return null;

    return container.querySelector(
      `[data-row="${position.posRow}"][data-col="${position.posCol}"]`
    );
  },

  /**
   * Get all cells for a board
   * @param {string} containerId
   * @returns {NodeList}
   */
  getAllCells(containerId) {
    const container = document.getElementById(containerId);
    return container ? container.querySelectorAll('.cell') : [];
  },

  // ============================================================================
  // Ship Display
  // ============================================================================

  /**
   * Place ship on board (visual only)
   * @param {string} containerId - Board container ID
   * @param {object} ship - Ship object with position, orientation, length
   */
  placeShip(containerId, ship) {
    const positions = this.getShipPositions(ship);
    
    console.log(`Placing ${ship.shipType} on board ${containerId}:`, positions);
    
    positions.forEach(pos => {
      const cell = this.getCell(containerId, pos);
      if (cell) {
        cell.classList.add('has-ship');
        cell.dataset.state = this.CELL_STATE.SHIP;
        cell.dataset.shipType = ship.shipType;
        console.log(`  Cell (${pos.posRow}, ${pos.posCol}) marked as ship`);
      } else {
        console.error(`  Cell not found at (${pos.posRow}, ${pos.posCol})`);
      }
    });
    
    console.log(`${ship.shipType} placement complete`);
  },

  /**
   * Remove ship from board (visual only)
   * @param {string} containerId
   * @param {object} ship
   */
  removeShip(containerId, ship) {
    const positions = this.getShipPositions(ship);
    
    positions.forEach(pos => {
      const cell = this.getCell(containerId, pos);
      if (cell) {
        cell.classList.remove('has-ship');
        cell.dataset.state = this.CELL_STATE.EMPTY;
        delete cell.dataset.shipType;
      }
    });
  },

  /**
   * Display entire fleet on board
   * @param {string} containerId
   * @param {Array} fleet - Array of 5 ships
   */
  displayFleet(containerId, fleet) {
    fleet.forEach(ship => this.placeShip(containerId, ship));
  },

  /**
   * Clear all ships from board
   * @param {string} containerId
   */
  clearShips(containerId) {
    const cells = this.getAllCells(containerId);
    cells.forEach(cell => {
      cell.classList.remove('has-ship');
      if (cell.dataset.state === this.CELL_STATE.SHIP) {
        cell.dataset.state = this.CELL_STATE.EMPTY;
      }
      delete cell.dataset.shipType;
    });
  },

  // ============================================================================
  // Attack Markers
  // ============================================================================

  /**
   * Mark cell as hit
   * @param {string} containerId
   * @param {{posRow: number, posCol: number}} position
   */
  markHit(containerId, position) {
    const cell = this.getCell(containerId, position);
    if (cell) {
      cell.classList.add('hit');
      cell.dataset.state = this.CELL_STATE.HIT;
      cell.textContent = '×';
    }
  },

  /**
   * Mark cell as miss
   * @param {string} containerId
   * @param {{posRow: number, posCol: number}} position
   */
  markMiss(containerId, position) {
    const cell = this.getCell(containerId, position);
    if (cell) {
      cell.classList.add('miss');
      cell.dataset.state = this.CELL_STATE.MISS;
      cell.textContent = '•';
    }
  },

  /**
   * Mark ship as sunk (highlight all ship cells)
   * @param {string} containerId
   * @param {object} ship
   */
  markSunk(containerId, ship) {
    const positions = this.getShipPositions(ship);
    
    positions.forEach(pos => {
      const cell = this.getCell(containerId, pos);
      if (cell) {
        cell.classList.add('sunk');
        cell.dataset.state = this.CELL_STATE.SUNK;
      }
    });
  },

  // ============================================================================
  // Cell Interactions
  // ============================================================================

  /**
   * Highlight cell on hover (for attack selection)
   * @param {string} containerId
   * @param {{posRow: number, posCol: number}} position
   */
  highlightCell(containerId, position) {
    const cell = this.getCell(containerId, position);
    if (cell && cell.dataset.state === this.CELL_STATE.EMPTY) {
      cell.classList.add('highlight');
    }
  },

  /**
   * Remove highlight from cell
   * @param {string} containerId
   * @param {{posRow: number, posCol: number}} position
   */
  unhighlightCell(containerId, position) {
    const cell = this.getCell(containerId, position);
    if (cell) {
      cell.classList.remove('highlight');
    }
  },

  /**
   * Enable attack clicks on board
   * @param {string} containerId
   * @param {function} callback - Called with {posRow, posCol} when cell clicked
   */
  enableAttacks(containerId, callback) {
    const container = document.getElementById(containerId);
    if (!container) return;

    container.classList.add('attackable');

    const cells = this.getAllCells(containerId);
    cells.forEach(cell => {
      // Only allow clicking empty cells (not already attacked)
      if (cell.dataset.state === this.CELL_STATE.EMPTY || 
          cell.dataset.state === this.CELL_STATE.SHIP) {
        
        cell.style.cursor = 'crosshair';
        
        cell.addEventListener('click', () => {
          const position = {
            posRow: parseInt(cell.dataset.row),
            posCol: parseInt(cell.dataset.col)
          };
          callback(position);
        });

        // Hover effect
        cell.addEventListener('mouseenter', () => {
          if (cell.dataset.state === this.CELL_STATE.EMPTY || 
              cell.dataset.state === this.CELL_STATE.SHIP) {
            cell.classList.add('highlight');
          }
        });

        cell.addEventListener('mouseleave', () => {
          cell.classList.remove('highlight');
        });
      }
    });
  },

  /**
   * Disable attack clicks on board
   * @param {string} containerId
   */
  disableAttacks(containerId) {
    const container = document.getElementById(containerId);
    if (!container) return;

    container.classList.remove('attackable');

    const cells = this.getAllCells(containerId);
    cells.forEach(cell => {
      cell.style.cursor = 'default';
      cell.replaceWith(cell.cloneNode(true)); // Remove all event listeners
    });
  },

  // ============================================================================
  // Helper Functions
  // ============================================================================

  /**
   * Get all positions occupied by a ship
   * @param {object} ship - {shipType, shipPosition, shipOrientation}
   * @returns {Array<{posRow: number, posCol: number}>}
   */
  getShipPositions(ship) {
    const positions = [];
    const lengths = {
      'Destroyer': 2,
      'Submarine': 3,
      'Cruiser': 3,
      'Battleship': 4,
      'Carrier': 5
    };
    
    const length = lengths[ship.shipType] || 0;
    const start = ship.shipPosition;

    for (let i = 0; i < length; i++) {
      if (ship.shipOrientation === 'Horizontal') {
        positions.push({
          posRow: start.posRow,
          posCol: start.posCol + i
        });
      } else { // Vertical
        positions.push({
          posRow: start.posRow + i,
          posCol: start.posCol
        });
      }
    }

    return positions;
  },

  /**
   * Clear entire board (reset to empty)
   * @param {string} containerId
   */
  clear(containerId) {
    const cells = this.getAllCells(containerId);
    cells.forEach(cell => {
      cell.className = 'cell';
      cell.dataset.state = this.CELL_STATE.EMPTY;
      cell.textContent = '';
      delete cell.dataset.shipType;
    });
  }
};

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
  module.exports = Board;
}
