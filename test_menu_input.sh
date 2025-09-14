#!/bin/bash
# Quick test script to verify menu input works

echo "Testing menu input functionality..."
echo "This will start the menu and automatically select option 2 (Exit)"

# Create a test input file
echo "2" > /tmp/menu_test_input

# Run the game with test input
cd /home/darlan/termina-hero-prolog2
timeout 10s swipl -g main -t halt app/main.pl < /tmp/menu_test_input

# Check exit code
if [ $? -eq 0 ]; then
    echo "✅ Menu input test PASSED - able to select option and exit properly"
else
    echo "❌ Menu input test FAILED"
fi

# Clean up
rm -f /tmp/menu_test_input

echo "Manual test: Run './run.sh' and try typing '1' or '2' to verify input works"
