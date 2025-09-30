#!/bin/bash

# Test-Automation Script Validation Runner
# Comprehensive validation for the updated test-automation.sh script

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "🔄 CrystalCog Test-Automation Script Validation"
echo "=============================================="
echo ""

# Test result tracking
tests_passed=0
tests_failed=0

run_validation_test() {
    local test_name="$1"
    local test_command="$2"
    
    echo -n "Testing $test_name... "
    
    # Use a more reliable approach for command evaluation
    if bash -c "$test_command" >/dev/null 2>&1; then
        echo -e "${GREEN}✅ PASS${NC}"
        ((tests_passed++))
        return 0
    else
        echo -e "${RED}❌ FAIL${NC}"
        ((tests_failed++))
        return 1
    fi
}

cd "$PROJECT_ROOT"

echo "1. Basic Functionality Tests"
echo "=============================="

# Test script syntax
run_validation_test "Script syntax validation" "bash -n tests/test-automation.sh"

# Test script is executable
run_validation_test "Script executable check" "[ -x tests/test-automation.sh ]"

# Test help functionality
run_validation_test "Help command" "tests/test-automation.sh --help"

# Test validate command
run_validation_test "Validate command" "tests/test-automation.sh validate"

echo ""
echo "2. Environment Adapter Tests"
echo "============================"

# Test environment adapter exists and is executable
run_validation_test "Environment adapter exists" "[ -x tests/environment-adapter.sh ]"

# Test Crystal check functionality
run_validation_test "Crystal availability check" "tests/environment-adapter.sh check-crystal"

# Test shards mock functionality
run_validation_test "Shards mock functionality" "tests/environment-adapter.sh test-shards"

echo ""
echo "3. Command Functionality Tests"
echo "=============================="

# Test auto-fix command
run_validation_test "Auto-fix command" "tests/test-automation.sh fix"

# Test validate infrastructure
run_validation_test "Infrastructure validation" "tests/test-automation.sh validate"

echo ""
echo "4. Security and Quality Tests"
echo "============================"

# Test shellcheck passes
if command -v shellcheck >/dev/null 2>&1; then
    run_validation_test "Shellcheck validation" "shellcheck tests/test-automation.sh"
else
    echo "Shellcheck not available - skipping"
fi

# Test required files exist
run_validation_test "Required test directories" "[ -d tests/unit ] && [ -d tests/integration ] && [ -d tests/performance ] && [ -d tests/functional ] && [ -d tests/reports ]"

# Test comprehensive test suite exists
run_validation_test "Comprehensive test suite" "[ -x tests/comprehensive-test-suite.sh ]"

# Test scripts directory has test-runner
run_validation_test "Test runner script" "[ -x scripts/test-runner.sh ]"

echo ""
echo "5. Dependency Compatibility Tests"
echo "================================="

# Test shard.yml exists
run_validation_test "Shard configuration" "[ -f shard.yml ]"

# Test basic project structure
run_validation_test "Source directory structure" "[ -d src ] && [ -d spec ]"

# Test main Crystal file exists
run_validation_test "Main Crystal file" "[ -f src/crystalcog.cr ]"

echo ""
echo "6. Documentation Tests"
echo "======================"

# Test README files exist
run_validation_test "Main README" "[ -f README.md ]"

# Test test documentation
run_validation_test "Test documentation" "[ -f tests/README.md ]"

# Test validation summary exists
run_validation_test "Validation summary" "[ -f docs/VALIDATION_SUMMARY.md ]"

echo ""
echo "7. Advanced Functionality Tests"
echo "==============================="

# Test error handling by trying invalid command
if tests/test-automation.sh invalid-command >/dev/null 2>&1; then
    echo -e "Error handling: ${RED}❌ FAIL${NC} (should reject invalid commands)"
    ((tests_failed++))
else
    echo -e "Error handling: ${GREEN}✅ PASS${NC}"
    ((tests_passed++))
fi

# Test that script runs from different directories
current_dir=$(pwd)
cd tests/
if ./test-automation.sh validate >/dev/null 2>&1; then
    echo -e "Directory independence: ${GREEN}✅ PASS${NC}"
    ((tests_passed++))
else
    echo -e "Directory independence: ${RED}❌ FAIL${NC}"
    ((tests_failed++))
fi
cd "$current_dir"

echo ""
echo "=========================================="
echo "VALIDATION SUMMARY"
echo "=========================================="
echo ""
echo "Tests passed: $tests_passed"
echo "Tests failed: $tests_failed"
echo "Total tests: $((tests_passed + tests_failed))"

if [ $tests_failed -eq 0 ]; then
    echo ""
    echo -e "${GREEN}✅ ALL VALIDATION TESTS PASSED!${NC}"
    echo ""
    echo "The test-automation.sh script has been successfully validated and is ready for use."
    echo ""
    echo "Key validations completed:"
    echo "- ✅ Script functionality and syntax"
    echo "- ✅ Environment adapter for missing dependencies"
    echo "- ✅ Security checks and code quality"
    echo "- ✅ Test infrastructure validation"
    echo "- ✅ Documentation completeness"
    echo "- ✅ Error handling and robustness"
    echo ""
    exit 0
else
    echo ""
    echo -e "${RED}❌ SOME TESTS FAILED${NC}"
    echo ""
    echo "Please review the failed tests above and address any issues."
    echo ""
    exit 1
fi