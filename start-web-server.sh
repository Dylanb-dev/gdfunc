#!/bin/bash
# Start the GDFunc web server
#
# Usage: ./start-web-server.sh [port]
#
# The web server will be available at http://localhost:3000 (or specified port)

set -e

PORT=${1:-3000}

echo "🚀 Starting GDFunc web server on port $PORT..."
echo "   Web UI will be available at http://localhost:$PORT"
echo ""

# Check if cabal is installed
if ! command -v cabal &> /dev/null; then
    echo "Error: cabal is not installed"
    echo "Please install GHC and cabal first"
    exit 1
fi

# Build and run the web server
# Note: May require --allow-newer for some dependencies with newer GHC versions
cabal run gdfunc-web --allow-newer
