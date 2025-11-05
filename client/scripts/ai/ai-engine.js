/**
 * AI Engine - Client-side game logic
 * Fixed AI fleet + Attack processing
 */

const AIEngine = {
  // AI Fleet (fixed positions)
  aiFleet: [
    {
      shipType: "Carrier",
      size: 5,
      positions: [
        { posRow: 0, posCol: 0 },
        { posRow: 0, posCol: 1 },
        { posRow: 0, posCol: 2 },
        { posRow: 0, posCol: 3 },
        { posRow: 0, posCol: 4 },
      ],
    },
    {
      shipType: "Battleship",
      size: 4,
      positions: [
        { posRow: 2, posCol: 0 },
        { posRow: 2, posCol: 1 },
        { posRow: 2, posCol: 2 },
        { posRow: 2, posCol: 3 },
      ],
    },
    {
      shipType: "Cruiser",
      size: 3,
      positions: [
        { posRow: 4, posCol: 0 },
        { posRow: 4, posCol: 1 },
        { posRow: 4, posCol: 2 },
      ],
    },
    {
      shipType: "Submarine",
      size: 3,
      positions: [
        { posRow: 6, posCol: 0 },
        { posRow: 6, posCol: 1 },
        { posRow: 6, posCol: 2 },
      ],
    },
    {
      shipType: "Destroyer",
      size: 2,
      positions: [
        { posRow: 8, posCol: 0 },
        { posRow: 8, posCol: 1 },
      ],
    },
  ],

  // Board state
  aiBoard: null, // AI's board (hidden from player)
  playerBoard: null, // Player's board (visible)

  // Initialize game
  initGame(playerFleet) {
    console.log("AIEngine.initGame called");
    console.log("Player fleet:", playerFleet);

    // Create 10x10 boards
    this.aiBoard = this.createBoard();
    this.playerBoard = this.createBoard();

    // Place AI ships on AI board
    this.aiFleet.forEach((ship) => {
      ship.positions.forEach((pos) => {
        this.aiBoard[pos.posRow][pos.posCol] = {
          hasShip: true,
          shipType: ship.shipType,
          attacked: false,
          hit: false,
        };
      });
    });

    // Place player ships on player board
    playerFleet.forEach((ship) => {
      ship.positions.forEach((pos) => {
        this.playerBoard[pos.posRow][pos.posCol] = {
          hasShip: true,
          shipType: ship.shipType,
          attacked: false,
          hit: false,
        };
      });
    });

    console.log("AIEngine initialized");
    console.log("AI board:", this.aiBoard);
    console.log("Player board:", this.playerBoard);
  },

  // Create empty 10x10 board
  createBoard() {
    const board = [];
    for (let row = 0; row < 10; row++) {
      board[row] = [];
      for (let col = 0; col < 10; col++) {
        board[row][col] = {
          hasShip: false,
          shipType: null,
          attacked: false,
          hit: false,
        };
      }
    }
    return board;
  },

  // Process player attack on AI board
  processPlayerAttack(position) {
    const { posRow, posCol } = position;
    const cell = this.aiBoard[posRow][posCol];

    if (cell.attacked) {
      return { result: "already_attacked", position };
    }

    cell.attacked = true;

    if (cell.hasShip) {
      cell.hit = true;

      // Check if ship is sunk
      const shipSunk = this.isShipSunk(this.aiBoard, cell.shipType);

      return {
        result: shipSunk ? "sunk" : "hit",
        position,
        shipType: cell.shipType,
        shipSunk,
      };
    } else {
      return {
        result: "miss",
        position,
      };
    }
  },

  // Process AI attack on player board
  processAIAttack() {
    // Simple random attack
    let posRow, posCol;
    do {
      posRow = Math.floor(Math.random() * 10);
      posCol = Math.floor(Math.random() * 10);
    } while (this.playerBoard[posRow][posCol].attacked);

    const cell = this.playerBoard[posRow][posCol];
    cell.attacked = true;

    if (cell.hasShip) {
      cell.hit = true;

      const shipSunk = this.isShipSunk(this.playerBoard, cell.shipType);

      return {
        result: shipSunk ? "sunk" : "hit",
        position: { posRow, posCol },
        shipType: cell.shipType,
        shipSunk,
      };
    } else {
      return {
        result: "miss",
        position: { posRow, posCol },
      };
    }
  },

  // Check if ship is sunk
  isShipSunk(board, shipType) {
    for (let row = 0; row < 10; row++) {
      for (let col = 0; col < 10; col++) {
        const cell = board[row][col];
        if (cell.hasShip && cell.shipType === shipType && !cell.hit) {
          return false; // Ship still has un-hit cells
        }
      }
    }
    return true; // All cells of this ship are hit
  },

  // Check if all ships are sunk
  allShipsSunk(board) {
    for (let row = 0; row < 10; row++) {
      for (let col = 0; col < 10; col++) {
        const cell = board[row][col];
        if (cell.hasShip && !cell.hit) {
          return false; // Found un-hit ship cell
        }
      }
    }
    return true; // All ships sunk
  },

  // Get board state (for debugging)
  getBoardState(board) {
    return board.map((row) =>
      row.map((cell) => ({
        hasShip: cell.hasShip,
        attacked: cell.attacked,
        hit: cell.hit,
      }))
    );
  },
};

console.log("AIEngine loaded");
