/**
 * Client-side validation for fleet placement
 * Per requirements: Validate when "Ready" button clicked
 */

const Validation = {
    // Ship types and their lengths (must match backend Game/Types.hs)
    SHIP_TYPES: {
        'Destroyer': 2,
        'Submarine': 3,
        'Cruiser': 3,
        'Battleship': 4,
        'Carrier': 5
    },

    BOARD_SIZE: 10, // 10x10 grid (0-9)

    // ============================================================================
    // Fleet Validation (Main Entry Point)
    // ============================================================================

    /**
     * Validate complete fleet (5 ships)
     * @param {Array} fleet - Array of ship objects
     * @returns {{valid: boolean, errors: Array<string>}}
     */
    validateFleet(fleet) {
        const errors = [];

        // Check 1: Must have exactly 5 ships
        if (fleet.length !== 5) {
            errors.push(`Must have exactly 5 ships (found ${fleet.length})`);
            return { valid: false, errors };
        }

        // Check 2: Correct ship types
        const typeCheck = this.checkShipTypes(fleet);
        if (!typeCheck.valid) {
            errors.push(...typeCheck.errors);
        }

        // Check 3: No overlaps
        const overlapCheck = this.checkOverlaps(fleet);
        if (!overlapCheck.valid) {
            errors.push(...overlapCheck.errors);
        }

        // Check 4: All ships in bounds
        const boundsCheck = this.checkBounds(fleet);
        if (!boundsCheck.valid) {
            errors.push(...boundsCheck.errors);
        }

        // Check 5: Valid orientations
        const orientationCheck = this.checkOrientations(fleet);
        if (!orientationCheck.valid) {
            errors.push(...orientationCheck.errors);
        }

        return {
            valid: errors.length === 0,
            errors
        };
    },

    // ============================================================================
    // Individual Checks
    // ============================================================================

    /**
     * Check if fleet has correct ship types (1 of each)
     * @param {Array} fleet
     * @returns {{valid: boolean, errors: Array<string>}}
     */
    checkShipTypes(fleet) {
        const errors = [];
        const requiredTypes = Object.keys(this.SHIP_TYPES);
        const foundTypes = fleet.map(ship => ship.shipType);

        // Check each required type
        requiredTypes.forEach(type => {
            const count = foundTypes.filter(t => t === type).length;
            if (count === 0) {
                errors.push(`Missing ship: ${type}`);
            } else if (count > 1) {
                errors.push(`Duplicate ship: ${type} (found ${count})`);
            }
        });

        // Check for invalid types
        foundTypes.forEach(type => {
            if (!requiredTypes.includes(type)) {
                errors.push(`Invalid ship type: ${type}`);
            }
        });

        return { valid: errors.length === 0, errors };
    },

    /**
     * Check if ships overlap (O(n²) - matches backend)
     * @param {Array} fleet
     * @returns {{valid: boolean, errors: Array<string>}}
     */
    checkOverlaps(fleet) {
        const errors = [];
        const allPositions = new Set();

        for (const ship of fleet) {
            const positions = this.getShipPositions(ship);
            
            for (const pos of positions) {
                const key = `${pos.posRow},${pos.posCol}`;
                if (allPositions.has(key)) {
                    errors.push(`Ships overlap at (${pos.posRow}, ${pos.posCol})`);
                }
                allPositions.add(key);
            }
        }

        return { valid: errors.length === 0, errors };
    },

    /**
     * Check if all ships are within board bounds (0-9)
     * @param {Array} fleet
     * @returns {{valid: boolean, errors: Array<string>}}
     */
    checkBounds(fleet) {
        const errors = [];

        for (const ship of fleet) {
            const positions = this.getShipPositions(ship);
            
            for (const pos of positions) {
                if (pos.posRow < 0 || pos.posRow >= this.BOARD_SIZE ||
                    pos.posCol < 0 || pos.posCol >= this.BOARD_SIZE) {
                    errors.push(`${ship.shipType} out of bounds at (${pos.posRow}, ${pos.posCol})`);
                }
            }
        }

        return { valid: errors.length === 0, errors };
    },

    /**
     * Check if all ships have valid orientations (Horizontal or Vertical)
     * @param {Array} fleet
     * @returns {{valid: boolean, errors: Array<string>}}
     */
    checkOrientations(fleet) {
        const errors = [];
        const validOrientations = ['Horizontal', 'Vertical'];

        for (const ship of fleet) {
            if (!validOrientations.includes(ship.shipOrientation)) {
                errors.push(`${ship.shipType} has invalid orientation: ${ship.shipOrientation}`);
            }
        }

        return { valid: errors.length === 0, errors };
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
        const length = this.SHIP_TYPES[ship.shipType];
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
     * Create empty fleet structure
     * @returns {Array} Empty fleet array
     */
    createEmptyFleet() {
        return [];
    },

    /**
     * Check if position is valid (0-9)
     * @param {{posRow: number, posCol: number}} position
     * @returns {boolean}
     */
    isValidPosition(position) {
        return position.posRow >= 0 && position.posRow < this.BOARD_SIZE &&
               position.posCol >= 0 && position.posCol < this.BOARD_SIZE;
    }
};

// Export for use in other modules
if (typeof module !== 'undefined' && module.exports) {
    module.exports = Validation;
}
