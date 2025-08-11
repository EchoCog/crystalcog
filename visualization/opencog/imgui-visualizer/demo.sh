#!/bin/bash
#
# OpenCog ImGui Visualizer Demo Script
# 
# This script demonstrates how to build and run the ImGui visualizer
# for OpenCog AtomSpace visualization.
#

set -e

echo "OpenCog ImGui Visualizer Demo"
echo "=============================="

# Check dependencies
echo "Checking dependencies..."

if ! command -v pkg-config &> /dev/null; then
    echo "Error: pkg-config not found"
    exit 1
fi

if ! pkg-config --exists glfw3; then
    echo "Error: GLFW3 not found. Please install libglfw3-dev"
    exit 1
fi

if ! pkg-config --exists gl; then
    echo "Error: OpenGL not found. Please install libgl1-mesa-dev"
    exit 1
fi

echo "Dependencies OK!"

# Build the test application
echo ""
echo "Building ImGui test application..."
cd "$(dirname "$0")/../../test"

if [ ! -d "build" ]; then
    mkdir build
fi

cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make

echo "Build completed successfully!"

# Run the demo
echo ""
echo "Running ImGui demo..."
echo "This will open a window showing the ImGui integration."
echo "Close the window to complete the demo."
echo ""

# Check if we can run with a display
if [ -n "$DISPLAY" ]; then
    echo "Running with display: $DISPLAY"
    ./imgui_test
else
    echo "No display detected. Running with virtual display..."
    Xvfb :99 -screen 0 1024x768x24 &
    XVFB_PID=$!
    export DISPLAY=:99
    sleep 2
    
    # Run for a short time in headless mode
    timeout 5s ./imgui_test || true
    
    # Clean up
    kill $XVFB_PID 2>/dev/null || true
    echo "Headless demo completed."
fi

echo ""
echo "Demo completed successfully!"
echo ""
echo "To integrate with OpenCog:"
echo "1. Build the full OpenCog system"
echo "2. Start a CogServer instance"  
echo "3. Run the OpenCogImGuiVisualizer"
echo "4. Connect to your CogServer to visualize AtomSpace data"
echo ""
echo "For more information, see the README.md file."