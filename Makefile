# Terminal Hero Makefile

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
	swipl -q -g "consult('app/main.pl'), main." -t halt

# Run the test suite
test:
	@echo "Running test suite..."
	swipl -q -g "consult('tests/tests.pl'), run_all_tests." -t halt

# Clean compiled files (if any)
clean:
	@echo "Cleaning compiled files..."
	find . -name "*.qlf" -delete
	find . -name "*.qly" -delete

# Alternative run command using the original file for compatibility
run-legacy:
	@echo "Running legacy version..."
	swipl -g play -t halt terminal-hero.pl
