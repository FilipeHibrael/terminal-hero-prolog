# Terminal Hero Makefile
# Provides convenient commands for building and running the game

.PHONY: run test clean help

# Default target
help:
	@echo "Terminal Hero - Available commands:"
	@echo "  make run     - Run the game"
	@echo "  make test    - Run the test suite"
	@echo "  make clean   - Clean compiled files"
	@echo "  make help    - Show this help"

# Run the main game
run:
	@echo "Starting Terminal Hero..."
	swipl -g main -t halt app/main.pl

# Run the test suite
test:
	@echo "Running test suite..."
	swipl -g run_tests -t halt tests/tests.pl

# Clean compiled files (if any)
clean:
	@echo "Cleaning compiled files..."
	find . -name "*.qlf" -delete
	find . -name "*.qly" -delete

# Alternative run command using the original file for compatibility
run-legacy:
	@echo "Running legacy version..."
	swipl -g play -t halt terminal-hero.pl
