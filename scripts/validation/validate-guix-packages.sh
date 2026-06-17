#!/bin/bash
# Simple validation script for Guix package definitions

echo "=== OpenCog Guix Package Validation ==="

# Check if package files exist
echo "Checking package files..."
if [ -f "gnu/packages/opencog.scm" ]; then
    echo "✓ opencog.scm exists"
else
    echo "✗ opencog.scm missing"
    exit 1
fi

if [ -f ".guix-channel" ]; then
    echo "✓ .guix-channel exists"
else
    echo "✗ .guix-channel missing"
    exit 1
fi

if [ -f "guix.scm" ]; then
    echo "✓ guix.scm manifest exists"
else
    echo "✗ guix.scm manifest missing"
    exit 1
fi

# Basic syntax check
echo -e "\nValidating Scheme syntax..."
if command -v guile > /dev/null; then
    echo "Testing package module syntax..."
    if guile -c "(use-modules (gnu packages opencog))" 2>/dev/null; then
        echo "✓ Package module syntax valid"
    else
        echo "✗ Package module syntax invalid"
        echo "Running syntax check..."
        guile -c "(use-modules (gnu packages opencog))"
    fi
    
    echo "Testing manifest syntax..."
    if guile -c "(load \"guix.scm\")" 2>/dev/null; then
        echo "✓ Manifest syntax valid"
    else
        echo "✗ Manifest syntax invalid"
        echo "Running syntax check..."
        guile -c "(load \"guix.scm\")"
    fi
else
    echo "⚠ Guile not available, skipping syntax validation"
    echo "To validate syntax, install Guile and run:"
    echo "  guile -c '(use-modules (gnu packages opencog))'"
    echo "  guile -c '(load \"guix.scm\")'"
fi

echo -e "\n=== Package Summary ==="
echo "Created the following OpenCog Guix packages:"
echo "  - cogutil: Core C++ utilities"
echo "  - atomspace: Hypergraph database and reasoning"
echo "  - opencog: Main cognitive architecture platform"
echo ""
echo "Usage:"
echo "  guix environment -m guix.scm    # Development environment"
echo "  guix install cogutil atomspace  # Install specific packages"
echo ""
echo "See README-GUIX.md for detailed usage instructions."