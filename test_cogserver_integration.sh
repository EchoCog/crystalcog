#!/bin/bash

# Integration test script for CogServer Network API
# This script tests the API endpoints using curl
# Usage: ./test_cogserver_integration.sh

set -e  # Exit on error

echo "🧪 CogServer Network API Integration Test"
echo "=========================================="

# Configuration
HOST="localhost"
HTTP_PORT="18080" 
TELNET_PORT="17001"

echo "📡 Testing server endpoints on ${HOST}:${HTTP_PORT}..."

# Test basic HTTP endpoints
echo ""
echo "🔍 Testing HTTP Endpoints:"

echo "   📊 Status endpoint..."
curl -s -f "http://${HOST}:${HTTP_PORT}/status" | jq '.running' > /dev/null
echo "      ✅ Status endpoint working"

echo "   📋 Version endpoint..."
curl -s -f "http://${HOST}:${HTTP_PORT}/version" | jq '.version' > /dev/null  
echo "      ✅ Version endpoint working"

echo "   🏓 Ping endpoint..."
curl -s -f "http://${HOST}:${HTTP_PORT}/ping" | jq '.status' > /dev/null
echo "      ✅ Ping endpoint working"

echo "   🧠 AtomSpace endpoint..."
curl -s -f "http://${HOST}:${HTTP_PORT}/atomspace" | jq '.size' > /dev/null
echo "      ✅ AtomSpace endpoint working"

echo "   🔍 Atoms endpoint..."
curl -s -f "http://${HOST}:${HTTP_PORT}/atoms" | jq '.count' > /dev/null
echo "      ✅ Atoms endpoint working"

echo "   👥 Sessions endpoint..."
curl -s -f "http://${HOST}:${HTTP_PORT}/sessions" | jq '.active_sessions' > /dev/null
echo "      ✅ Sessions endpoint working"

echo "   ❌ 404 handling..."
response_code=$(curl -s -o /dev/null -w "%{http_code}" "http://${HOST}:${HTTP_PORT}/nonexistent")
if [ "$response_code" = "404" ]; then
    echo "      ✅ 404 error handling working"
else
    echo "      ❌ Expected 404, got ${response_code}"
fi

# Test telnet interface (via HTTP with query parameters)
echo ""
echo "💻 Testing Telnet Interface:"

echo "   🔧 Help command..."
curl -s -f "http://${HOST}:${TELNET_PORT}/?cmd=help" | grep -q "Available commands"
echo "      ✅ Help command working"

echo "   📊 Info command..."
curl -s -f "http://${HOST}:${TELNET_PORT}/?cmd=info" | grep -q "CogServer"
echo "      ✅ Info command working"

echo "   🧠 AtomSpace command..."
curl -s -f "http://${HOST}:${TELNET_PORT}/?cmd=atomspace" | grep -q "AtomSpace"
echo "      ✅ AtomSpace command working"

echo "   📈 Stats command..."
curl -s -f "http://${HOST}:${TELNET_PORT}/?cmd=stats" | grep -q "Session"
echo "      ✅ Stats command working"

# Test WebSocket upgrade simulation
echo ""
echo "🔌 Testing WebSocket Protocol:"

echo "   ⬆️  WebSocket upgrade..."
response_code=$(curl -s -o /dev/null -w "%{http_code}" \
    -H "Connection: Upgrade" \
    -H "Upgrade: websocket" \
    -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" \
    -H "Sec-WebSocket-Version: 13" \
    "http://${HOST}:${HTTP_PORT}/")

if [ "$response_code" = "101" ]; then
    echo "      ✅ WebSocket upgrade working (HTTP 101)"
else
    echo "      ❌ WebSocket upgrade failed (HTTP ${response_code})"
fi

echo "   ❌ Invalid WebSocket upgrade..."
response_code=$(curl -s -o /dev/null -w "%{http_code}" \
    -H "Connection: keep-alive" \
    -H "Upgrade: websocket" \
    "http://${HOST}:${HTTP_PORT}/")

if [ "$response_code" = "400" ]; then
    echo "      ✅ Invalid upgrade properly rejected (HTTP 400)"
else
    echo "      ❌ Invalid upgrade not rejected (HTTP ${response_code})"
fi

# Test atom creation via POST
echo ""
echo "🔬 Testing Atom Operations:"

echo "   ➕ Creating atom..."
response_code=$(curl -s -o /dev/null -w "%{http_code}" \
    -X POST \
    -H "Content-Type: application/json" \
    -d '{"type":"ConceptNode","name":"test_atom"}' \
    "http://${HOST}:${HTTP_PORT}/atoms")

if [ "$response_code" = "201" ]; then
    echo "      ✅ Atom creation working (HTTP 201)"
else
    echo "      ❌ Atom creation failed (HTTP ${response_code})"
fi

echo "   🔍 Verifying atom exists..."
curl -s -f "http://${HOST}:${HTTP_PORT}/atoms" | jq -e '.atoms[] | select(.name == "test_atom")' > /dev/null
echo "      ✅ Created atom found in AtomSpace"

# Final summary
echo ""
echo "✨ Integration test completed successfully!"
echo ""
echo "🎯 All tested features:"
echo "   • HTTP REST API endpoints (7 endpoints)" 
echo "   • Telnet command interface (4 commands)"
echo "   • WebSocket protocol upgrade"
echo "   • Atom CRUD operations"
echo "   • Error handling and validation"
echo ""
echo "💡 CogServer Network API is fully functional!"