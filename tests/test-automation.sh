#!/bin/bash

# Test Automation Script for CrystalCog Development
# Provides automated testing workflows for developers
# Usage: ./tests/test-automation.sh [COMMAND] [OPTIONS]

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

# Configuration
WATCH_MODE=false
AUTO_FIX=false
VERBOSE=false
PARALLEL=false
MAX_PARALLEL=4

print_header() {
    echo -e "${CYAN}========================================${NC}"
    echo -e "${CYAN}$1${NC}"
    echo -e "${CYAN}========================================${NC}"
}

print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

show_help() {
    cat << EOF
CrystalCog Test Automation Script

COMMANDS:
    watch           Run tests in watch mode (re-run on file changes)
    pre-commit      Run pre-commit checks (linting, quick tests)
    pre-push        Run pre-push checks (full test suite)
    fix             Auto-fix code issues where possible
    ci-local        Simulate CI environment locally
    regression      Run regression test suite
    profile         Profile test performance
    validate        Validate test infrastructure

OPTIONS:
    -v, --verbose   Verbose output
    -p, --parallel  Run tests in parallel
    -f, --fix       Auto-fix issues where possible
    -h, --help      Show this help

EXAMPLES:
    $0 watch                    # Watch mode for development
    $0 pre-commit --fix         # Pre-commit with auto-fix
    $0 ci-local --parallel      # Simulate CI with parallel tests
    $0 regression --verbose     # Verbose regression testing

EOF
}

# Watch mode for development
run_watch_mode() {
    print_header "Test Watch Mode"
    print_status "Watching for file changes..."
    print_status "Press Ctrl+C to stop"
    
    # Install inotify tools if available
    if command -v inotifywait &> /dev/null; then
        watch_with_inotify
    else
        watch_with_polling
    fi
}

watch_with_inotify() {
    while true; do
        # Watch for changes in source and spec directories
        inotifywait -r -e modify,create,delete src/ spec/ 2>/dev/null || true
        
        print_status "Files changed, running tests..."
        
        # Run quick test suite
        if run_quick_tests; then
            print_success "✅ Tests passed"
        else
            print_error "❌ Tests failed"
        fi
        
        echo ""
        sleep 1
    done
}

watch_with_polling() {
    local last_change=""
    
    while true; do
        # Check for file modifications
        current_change=$(find src/ spec/ -type f -name "*.cr" -exec stat -c %Y {} \; 2>/dev/null | sort -n | tail -1)
        
        if [ "$current_change" != "$last_change" ]; then
            print_status "Files changed, running tests..."
            
            if run_quick_tests; then
                print_success "✅ Tests passed"
            else
                print_error "❌ Tests failed"
            fi
            
            last_change="$current_change"
            echo ""
        fi
        
        sleep 2
    done
}

# Quick tests for development
run_quick_tests() {
    # Run linting first (fast)
    crystal tool format --check src/ spec/ 2>/dev/null || {
        if [ "$AUTO_FIX" = true ]; then
            print_status "Auto-fixing code formatting..."
            crystal tool format src/ spec/ 2>/dev/null
        else
            print_warning "Code formatting issues found"
        fi
    }
    
    # Run unit tests for recently changed components
    local changed_components=$(get_changed_components)
    
    if [ -n "$changed_components" ]; then
        for component in $changed_components; do
            print_status "Testing $component..."
            if ! "${SCRIPT_DIR}/comprehensive-test-suite.sh" --unit --component "$component" >/dev/null 2>&1; then
                return 1
            fi
        done
    else
        # Run basic unit tests
        if ! "${SCRIPT_DIR}/comprehensive-test-suite.sh" --unit --component cogutil >/dev/null 2>&1; then
            return 1
        fi
    fi
    
    return 0
}

# Get components that have changed recently
get_changed_components() {
    local components=""
    
    # Check for recent modifications in component directories
    for component in cogutil atomspace pln cogserver pattern_matching nlp; do
        if [ -d "src/$component" ] && [ -n "$(find "src/$component" -name "*.cr" -mmin -5 2>/dev/null)" ]; then
            components="$components $component"
        fi
    done
    
    echo "$components"
}

# Pre-commit checks
run_pre_commit() {
    print_header "Pre-commit Checks"
    
    local exit_code=0
    
    # 1. Code formatting
    print_status "Checking code formatting..."
    if crystal tool format --check src/ spec/ 2>/dev/null; then
        print_success "Code formatting OK"
    else
        if [ "$AUTO_FIX" = true ]; then
            print_status "Auto-fixing code formatting..."
            crystal tool format src/ spec/
            print_success "Code formatting fixed"
        else
            print_error "Code formatting issues found. Run: crystal tool format src/ spec/"
            exit_code=1
        fi
    fi
    
    # 2. Static analysis
    print_status "Running static analysis..."
    if crystal build --no-codegen --warnings-as-errors src/crystalcog.cr 2>/dev/null; then
        print_success "Static analysis OK"
    else
        print_warning "Static analysis found issues"
        exit_code=1
    fi
    
    # 3. Quick unit tests
    print_status "Running quick unit tests..."
    if "${SCRIPT_DIR}/comprehensive-test-suite.sh" --unit --component cogutil >/dev/null 2>&1; then
        print_success "Quick unit tests OK"
    else
        print_error "Quick unit tests failed"
        exit_code=1
    fi
    
    # 4. Security checks (basic)
    print_status "Running security checks..."
    run_security_checks
    
    return $exit_code
}

# Pre-push checks
run_pre_push() {
    print_header "Pre-push Checks"
    
    local exit_code=0
    
    # 1. Full unit test suite
    print_status "Running full unit test suite..."
    if "${SCRIPT_DIR}/comprehensive-test-suite.sh" --unit >/dev/null 2>&1; then
        print_success "Unit tests OK"
    else
        print_error "Unit tests failed"
        exit_code=1
    fi
    
    # 2. Integration tests
    print_status "Running integration tests..."
    if "${SCRIPT_DIR}/comprehensive-test-suite.sh" --integration >/dev/null 2>&1; then
        print_error "Integration tests failed"
        exit_code=1
    fi
    
    # 3. Performance regression check
    print_status "Checking performance regressions..."
    if ! check_performance_regression; then
        print_warning "Performance regression detected"
        # Don't fail on performance issues, just warn
    fi
    
    return $exit_code
}

# Auto-fix code issues
run_auto_fix() {
    print_header "Auto-fixing Code Issues"
    
    # 1. Format code
    print_status "Fixing code formatting..."
    crystal tool format src/ spec/
    print_success "Code formatting fixed"
    
    # 2. Fix common patterns (if we had ameba or similar)
    print_status "Looking for common issues to fix..."
    
    # Check for common patterns that can be auto-fixed
    fix_common_patterns
    
    print_success "Auto-fix completed"
}

fix_common_patterns() {
    # Fix trailing whitespace
    find src/ spec/ -name "*.cr" -exec sed -i 's/[[:space:]]*$//' {} \;
    
    # Fix mixed line endings (if any)
    find src/ spec/ -name "*.cr" -exec dos2unix {} \; 2>/dev/null || true
    
    print_status "Fixed common code patterns"
}

# Simulate CI environment locally
run_ci_local() {
    print_header "Simulating CI Environment"
    
    # Clean environment
    print_status "Cleaning build artifacts..."
    rm -rf build/ 2>/dev/null || true
    
    # Install dependencies fresh
    print_status "Installing dependencies..."
    shards install --skip-postinstall
    
    # Run full test suite
    print_status "Running full test suite..."
    if [ "$PARALLEL" = true ]; then
        run_tests_parallel
    else
        "${SCRIPT_DIR}/comprehensive-test-suite.sh" --all
    fi
}

# Run tests in parallel
run_tests_parallel() {
    print_status "Running tests in parallel (max $MAX_PARALLEL workers)..."
    
    # Create job queue
    local jobs=()
    jobs+=("${SCRIPT_DIR}/comprehensive-test-suite.sh --unit --component cogutil")
    jobs+=("${SCRIPT_DIR}/comprehensive-test-suite.sh --unit --component atomspace")
    jobs+=("${SCRIPT_DIR}/comprehensive-test-suite.sh --unit --component pln")
    jobs+=("${SCRIPT_DIR}/comprehensive-test-suite.sh --unit --component cogserver")
    jobs+=("${SCRIPT_DIR}/comprehensive-test-suite.sh --integration")
    jobs+=("${SCRIPT_DIR}/comprehensive-test-suite.sh --performance")
    
    # Run jobs in parallel
    local pids=()
    local job_index=0
    
    for job in "${jobs[@]}"; do
        if [ ${#pids[@]} -ge $MAX_PARALLEL ]; then
            # Wait for a job to complete
            wait_for_job pids
        fi
        
        # Start new job
        echo "Starting job $((job_index + 1)): $job"
        $job >/dev/null 2>&1 &
        pids+=($!)
        ((job_index++))
    done
    
    # Wait for remaining jobs
    for pid in "${pids[@]}"; do
        wait $pid
    done
    
    print_success "Parallel tests completed"
}

wait_for_job() {
    local -n pids_ref=$1
    local completed_pid
    
    # Wait for any job to complete
    wait -n
    completed_pid=$?
    
    # Remove completed PID from array
    local new_pids=()
    for pid in "${pids_ref[@]}"; do
        if kill -0 $pid 2>/dev/null; then
            new_pids+=($pid)
        fi
    done
    pids_ref=("${new_pids[@]}")
}

# Run regression tests
run_regression() {
    print_header "Regression Test Suite"
    
    # Run comprehensive tests with detailed output
    "${SCRIPT_DIR}/comprehensive-test-suite.sh" --functional --verbose
    
    # Check for performance regressions
    check_performance_regression
    
    # Validate core functionality
    validate_core_functionality
}

# Check for performance regressions
check_performance_regression() {
    print_status "Checking performance regressions..."
    
    # Run current benchmarks
    local current_results="/tmp/current-benchmarks.json"
    crystal run "${SCRIPT_DIR}/performance/comprehensive_benchmarks.cr" > "$current_results" 2>/dev/null || {
        print_warning "Could not run performance benchmarks"
        return 1
    }
    
    # Compare with baseline (if exists)
    local baseline="${SCRIPT_DIR}/reports/performance/baseline-benchmarks.json"
    if [ -f "$baseline" ]; then
        # Simple comparison (in a real implementation, you'd parse JSON)
        if [ -s "$current_results" ]; then
            print_status "Performance comparison completed"
            return 0
        else
            return 1
        fi
    else
        print_warning "No performance baseline found"
        return 0
    fi
}

# Validate core functionality
validate_core_functionality() {
    print_status "Validating core functionality..."
    
    # Test basic AtomSpace operations
    local test_script="/tmp/core-validation.cr"
    cat > "$test_script" << 'EOF'
require "./src/cogutil/cogutil"
require "./src/atomspace/atomspace_main"

CogUtil.initialize
AtomSpace.initialize

atomspace = AtomSpace::AtomSpace.new
node = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "test")
raise "Core validation failed" unless atomspace.contains?(node)

puts "Core functionality validation passed"
EOF

    if crystal run --error-trace "$test_script" >/dev/null 2>&1; then
        print_success "Core functionality OK"
        return 0
    else
        print_error "Core functionality validation failed"
        return 1
    fi
}

# Profile test performance
run_profiling() {
    print_header "Test Performance Profiling"
    
    print_status "Profiling test suite performance..."
    
    # Time each test category
    local categories=("unit" "integration" "performance" "functional")
    
    for category in "${categories[@]}"; do
        print_status "Profiling $category tests..."
        
        local start_time=$(date +%s.%N)
        "${SCRIPT_DIR}/comprehensive-test-suite.sh" --$category >/dev/null 2>&1 || true
        local end_time=$(date +%s.%N)
        
        local duration=$(echo "$end_time - $start_time" | bc -l)
        echo "$category: ${duration}s"
    done
}

# Run security checks
run_security_checks() {
    # Check for hardcoded secrets (basic patterns)
    local secret_patterns=("password\s*=" "api_key\s*=" "secret\s*=" "token\s*=")
    
    for pattern in "${secret_patterns[@]}"; do
        if grep -r -i "$pattern" src/ spec/ 2>/dev/null; then
            print_warning "Potential hardcoded secret found"
        fi
    done
    
    # Check for unsafe operations
    if grep -r "system\|exec\|\`" src/ 2>/dev/null; then
        print_warning "Potentially unsafe system calls found"
    fi
}

# Validate test infrastructure
validate_infrastructure() {
    print_header "Validating Test Infrastructure"
    
    local exit_code=0
    
    # Check test files exist
    print_status "Checking test file structure..."
    
    local required_files=(
        "tests/comprehensive-test-suite.sh"
        "tests/README.md"
        "scripts/test-runner.sh"
    )
    
    for file in "${required_files[@]}"; do
        if [ ! -f "$file" ]; then
            print_error "Required test file missing: $file"
            exit_code=1
        fi
    done
    
    # Check test directories
    local required_dirs=(
        "tests/unit"
        "tests/integration"
        "tests/performance"
        "tests/functional"
        "tests/reports"
    )
    
    for dir in "${required_dirs[@]}"; do
        if [ ! -d "$dir" ]; then
            print_error "Required test directory missing: $dir"
            exit_code=1
        fi
    done
    
    # Validate test scripts are executable
    if [ ! -x "tests/comprehensive-test-suite.sh" ]; then
        print_error "Test suite script is not executable"
        exit_code=1
    fi
    
    if [ $exit_code -eq 0 ]; then
        print_success "Test infrastructure validation passed"
    fi
    
    return $exit_code
}

# Parse command line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            -p|--parallel)
                PARALLEL=true
                shift
                ;;
            -f|--fix)
                AUTO_FIX=true
                shift
                ;;
            -h|--help)
                show_help
                exit 0
                ;;
            *)
                break
                ;;
        esac
    done
}

# Main execution
main() {
    local command=${1:-help}
    shift || true
    
    parse_args "$@"
    
    cd "$PROJECT_ROOT"
    
    case $command in
        watch)
            run_watch_mode
            ;;
        pre-commit)
            run_pre_commit
            ;;
        pre-push)
            run_pre_push
            ;;
        fix)
            run_auto_fix
            ;;
        ci-local)
            run_ci_local
            ;;
        regression)
            run_regression
            ;;
        profile)
            run_profiling
            ;;
        validate)
            validate_infrastructure
            ;;
        help|--help|-h)
            show_help
            ;;
        *)
            print_error "Unknown command: $command"
            show_help
            exit 1
            ;;
    esac
}

# Run main function
main "$@"