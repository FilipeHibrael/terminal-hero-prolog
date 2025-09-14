#!/bin/bash
# Terminal Hero run script

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}Terminal Hero - Modular Version${NC}"
echo "================================="

# Check if SWI-Prolog is installed
if ! command -v swipl &> /dev/null; then
    echo -e "${RED}Error: SWI-Prolog is not installed.${NC}"
    echo "Please install it using:"
    echo "  sudo apt-get install swi-prolog  (Ubuntu/Debian)"
    echo "  sudo dnf install swi-prolog      (Fedora)"
    exit 1
fi

# Handle command line arguments
case "${1:-run}" in
    "run"|"start"|"play")
        echo -e "${GREEN}Starting Terminal Hero...${NC}"
        swipl -g main -t halt app/main.pl
        ;;
    "test"|"tests")
        echo -e "${GREEN}Running test suite...${NC}"
        swipl -g run_tests -t halt tests/tests.pl
        ;;
    "legacy"|"original")
        echo -e "${GREEN}Running legacy version...${NC}"
        swipl -g play -t halt terminal-hero.pl
        ;;
    "interactive"|"repl")
        echo -e "${GREEN}Starting interactive mode...${NC}"
        echo "Type 'main.' to start the game."
        swipl app/main.pl
        ;;
    "help"|"-h"|"--help")
        echo "Usage: $0 [command]"
        echo ""
        echo "Commands:"
        echo "  run, start, play    - Run the modular game (default)"
        echo "  test, tests         - Run the test suite"
        echo "  legacy, original    - Run the original monolithic version"
        echo "  interactive, repl   - Start interactive Prolog session"
        echo "  help                - Show this help message"
        ;;
    *)
        echo -e "${RED}Unknown command: $1${NC}"
        echo "Use '$0 help' for available commands."
        exit 1
        ;;
esac
