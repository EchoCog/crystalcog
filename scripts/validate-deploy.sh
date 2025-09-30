#!/bin/bash
# CrystalCog Deploy Script Quick Validation
# Use this script for ongoing validation of the deployment script

set -e

# Configuration
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_PATH="$PROJECT_ROOT/scripts/production/deploy.sh"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "🔄 CrystalCog Deploy Script Quick Validation"
echo "============================================"
echo ""

# Quick validation tests
tests_passed=0
tests_failed=0

run_test() {
    local test_name="$1"
    local test_command="$2"
    
    if eval "$test_command" >/dev/null 2>&1; then
        echo -e "   ${GREEN}✅ PASS${NC}: $test_name"
        ((tests_passed++))
    else
        echo -e "   ${RED}❌ FAIL${NC}: $test_name"
        ((tests_failed++))
    fi
}

# Core validation tests
run_test "Script syntax" "bash -n '$SCRIPT_PATH'"
run_test "Script executable" "[ -x '$SCRIPT_PATH' ]"
run_test "Help function" "'$SCRIPT_PATH' --help"
run_test "Docker Compose file" "[ -f '$PROJECT_ROOT/docker-compose.production.yml' ]"
run_test "Health check script" "bash -n '$PROJECT_ROOT/scripts/production/healthcheck.sh'"
run_test "Guix manifest" "[ -f '$PROJECT_ROOT/guix.scm' ]"
run_test "Config directory" "[ -d '$PROJECT_ROOT/config/production' ]"
run_test "Core functions defined" "grep -q '^main()' '$SCRIPT_PATH' && grep -q '^deploy()' '$SCRIPT_PATH'"

echo ""
echo "Summary: $tests_passed passed, $tests_failed failed"

if [ $tests_failed -eq 0 ]; then
    echo -e "${GREEN}✅ All validation tests passed!${NC}"
    exit 0
else
    echo -e "${RED}❌ Some tests failed. Run full validation for details.${NC}"
    exit 1
fi