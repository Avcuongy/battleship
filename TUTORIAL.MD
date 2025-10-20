# Di chuyển vào thư mục server
cd server

# Clean build artifacts (nếu có)
stack clean

# Generate .cabal file từ package.yaml
stack build --dry-run

# Install dependencies
stack build --only-dependencies

# Build project
stack build

# Run tests
stack test

# Run tests with verbose output
stack test --test-arguments "--format=progress"

# Run specific test
stack test --test-arguments "-m Game.Board"

# Run with coverage
stack test --coverage

# Watch mode (auto-rerun on file change)
stack test --file-watch --fast
