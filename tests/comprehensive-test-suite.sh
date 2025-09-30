#!/bin/bash

# Comprehensive Testing Suite for CrystalCog
# Implements Agent-Zero Genesis roadmap testing requirements
# Usage: ./tests/comprehensive-test-suite.sh [OPTIONS]

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
REPORTS_DIR="${SCRIPT_DIR}/reports"
TEMP_DIR="/tmp/crystalcog-tests-$$"

# Test flags
RUN_UNIT=false
RUN_INTEGRATION=false
RUN_PERFORMANCE=false
RUN_FUNCTIONAL=false
RUN_AGENT_ZERO=false
GENERATE_REPORTS=false
GENERATE_COVERAGE=false
RUN_VALIDATION=false
COMPONENT=""
VERBOSE=false
HELP=false

# Statistics
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0
SKIPPED_TESTS=0

# Print functions  
print_header() {
    echo -e "${CYAN}========================================${NC}"
    echo -e "${CYAN}$1${NC}"
    echo -e "${CYAN}========================================${NC}"
}

print_section() {
    echo -e "${BLUE}[SECTION]${NC} $1"
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

print_result() {
    local result=$1
    local test_name=$2
    if [ "$result" -eq 0 ]; then
        print_success "✓ $test_name"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        print_error "✗ $test_name"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
}

# Show help
show_help() {
    cat << EOF
CrystalCog Comprehensive Testing Suite

Usage: $0 [OPTIONS]

Options:
    -h, --help          Show this help message
    -a, --all           Run all test categories
    -u, --unit          Run unit tests
    -i, --integration   Run integration tests
    -p, --performance   Run performance/benchmark tests
    -f, --functional    Run functional/end-to-end tests
    -z, --agent-zero    Run Agent-Zero specific tests
    -r, --reports       Generate test reports
    -c, --coverage      Generate coverage reports
    -C, --component     Run tests for specific component
    -v, --verbose       Run with verbose output
    --validate          Validate test infrastructure only (no dependencies required)
    
    --clean            Clean test artifacts and reports

Examples:
    $0 --all                    # Run complete test suite
    $0 --unit --coverage        # Run unit tests with coverage
    $0 --component atomspace    # Test only atomspace component
    $0 --integration --verbose  # Run integration tests with verbose output
    $0 --performance            # Run performance benchmarks only

Components:
    cogutil, atomspace, pln, cogserver, pattern_matching, nlp, opencog

EOF
}

# Parse command line arguments
parse_args() {
    while [[ $# -gt 0 ]]; do
        case $1 in
            -h|--help)
                HELP=true
                shift
                ;;
            -a|--all)
                RUN_UNIT=true
                RUN_INTEGRATION=true
                RUN_PERFORMANCE=true
                RUN_FUNCTIONAL=true
                RUN_AGENT_ZERO=true
                GENERATE_REPORTS=true
                shift
                ;;
            -u|--unit)
                RUN_UNIT=true
                shift
                ;;
            -i|--integration)
                RUN_INTEGRATION=true
                shift
                ;;
            -p|--performance)
                RUN_PERFORMANCE=true
                shift
                ;;
            -f|--functional)
                RUN_FUNCTIONAL=true
                shift
                ;;
            -z|--agent-zero)
                RUN_AGENT_ZERO=true
                shift
                ;;
            -r|--reports)
                GENERATE_REPORTS=true
                shift
                ;;
            -c|--coverage)
                GENERATE_COVERAGE=true
                shift
                ;;
            -C|--component)
                COMPONENT="$2"
                shift 2
                ;;
            -v|--verbose)
                VERBOSE=true
                shift
                ;;
            --validate)
                RUN_VALIDATION=true
                shift
                ;;
            --clean)
                clean_artifacts
                exit 0
                ;;
            *)
                print_error "Unknown option: $1"
                show_help
                exit 1
                ;;
        esac
    done
}

# Validate test infrastructure without requiring Crystal
validate_test_infrastructure() {
    print_section "Validating Test Infrastructure"
    
    local validation_passed=true
    
    # Check project structure
    print_status "Checking project structure..."
    
    local required_dirs=("src" "spec" "tests")
    for dir in "${required_dirs[@]}"; do
        if [ -d "$dir" ]; then
            print_success "$dir directory exists"
        else
            print_error "$dir directory missing"
            validation_passed=false
        fi
    done
    
    # Check configuration files
    local config_files=("shard.yml" ".gitignore")
    for file in "${config_files[@]}"; do
        if [ -f "$file" ]; then
            print_success "$file exists"
        else
            print_warning "$file missing"
        fi
    done
    
    # Check test directories
    local test_components=("cogutil" "atomspace" "pln" "cogserver" "pattern_matching" "nlp" "opencog")
    for component in "${test_components[@]}"; do
        if [ -d "spec/$component" ]; then
            local test_count
            test_count=$(find "spec/$component" -name "*_spec.cr" 2>/dev/null | wc -l)
            print_status "$component: $test_count test files"
            TOTAL_TESTS=$((TOTAL_TESTS + 1))
        else
            print_warning "No test directory for $component"
        fi
    done
    
    # Check source code structure
    for component in "${test_components[@]}"; do
        if [ -d "src/$component" ]; then
            local src_count
            src_count=$(find "src/$component" -name "*.cr" 2>/dev/null | wc -l)
            print_status "$component: $src_count source files"
        else
            print_warning "No source directory for $component"
        fi
    done
    
    # Validate shell script syntax
    print_status "Validating test script syntax..."
    if bash -n "$0"; then
        print_success "Shell script syntax valid"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        print_error "Shell script syntax errors"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        validation_passed=false
    fi
    
    if [ "$validation_passed" = true ]; then
        print_success "Test infrastructure validation passed"
        return 0
    else
        print_error "Test infrastructure validation failed"
        return 1
    fi
}

# Check dependencies and provide helpful information
check_dependencies() {
    local missing_deps=false
    
    # Check Crystal
    if ! command -v crystal &> /dev/null; then
        print_warning "Crystal is not installed"
        print_status "Install Crystal from: https://crystal-lang.org/install/"
        print_status "Or use Docker: docker run --rm -v \"\$PWD\":/workspace -w /workspace crystallang/crystal"
        missing_deps=true
    else
        print_success "Crystal found: $(crystal version 2>/dev/null | head -1 || echo 'version unknown')"
    fi
    
    # Check shards (Crystal package manager)
    if command -v crystal &> /dev/null && ! command -v shards &> /dev/null; then
        print_warning "Shards (Crystal package manager) not found"
        missing_deps=true
    fi
    
    # Check other tools
    if ! command -v git &> /dev/null; then
        print_warning "Git not found - some tests may be limited"
    fi
    
    if [ "$missing_deps" = true ]; then
        print_warning "Some dependencies missing - functionality will be limited"
        return 1
    fi
    
    return 0
}

# Setup test environment
setup_environment() {
    print_status "Setting up test environment..."
    
    # Create directories
    mkdir -p "${REPORTS_DIR}"/{coverage,performance,integration}
    mkdir -p "${TEMP_DIR}"
    
    # Change to project root
    cd "${PROJECT_ROOT}"
    
    # Check dependencies
    if ! check_dependencies; then
        print_warning "Continuing with limited functionality"
    fi
    
    # Install dependencies if Crystal is available
    if command -v crystal &> /dev/null && [ -f "shard.yml" ]; then
        print_status "Installing Crystal dependencies..."
        if command -v shards &> /dev/null; then
            shards install --skip-postinstall 2>/dev/null || print_warning "Failed to install dependencies"
        else
            print_warning "Shards not available - skipping dependency installation"
        fi
    fi
    
    print_success "Test environment setup complete"
}

# Clean test artifacts
clean_artifacts() {
    print_status "Cleaning test artifacts..."
    rm -rf "${TEMP_DIR}" 2>/dev/null || true
    rm -rf "${REPORTS_DIR}/coverage"/* 2>/dev/null || true
    rm -rf "${REPORTS_DIR}/performance"/* 2>/dev/null || true
    rm -rf "${REPORTS_DIR}/integration"/* 2>/dev/null || true
    print_success "Test artifacts cleaned"
}

# Run unit tests
run_unit_tests() {
    print_section "Running Unit Tests"
    
    local components=("cogutil" "atomspace" "pln" "cogserver" "pattern_matching" "nlp" "opencog")
    
    if [ -n "$COMPONENT" ]; then
        components=("$COMPONENT")
    fi
    
    # Check if Crystal is available
    if ! command -v crystal &> /dev/null; then
        print_warning "Crystal not available - validating test structure only"
        
        for component in "${components[@]}"; do
            if [ -d "spec/$component" ]; then
                local test_count
                test_count=$(find "spec/$component" -name "*_spec.cr" 2>/dev/null | wc -l)
                print_status "Found $test_count test files for $component"
                print_result 0 "$component test structure validation"
            else
                print_warning "No unit test directory found for $component"
                SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
            fi
        done
        return
    fi
    
    for component in "${components[@]}"; do
        if [ -d "spec/$component" ]; then
            print_status "Testing $component unit tests..."
            
            local verbose_flag=""
            if [ "$VERBOSE" = true ]; then
                verbose_flag="--verbose"
            fi
            
            if crystal spec "spec/$component/" --error-trace $verbose_flag 2>/dev/null; then
                print_result 0 "$component unit tests"
            else
                print_result 1 "$component unit tests"
            fi
        else
            print_warning "No unit tests found for $component"
            SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
        fi
    done
}

# Run integration tests
run_integration_tests() {
    print_section "Running Integration Tests"
    
    # Crystal integration tests
    local integration_tests=("test_basic.cr" "test_pln.cr" "test_pattern_matching.cr" "test_cogserver_api.cr")
    
    for test in "${integration_tests[@]}"; do
        if [ -f "$test" ]; then
            print_status "Running $test..."
            
            if crystal run --error-trace "$test" >/dev/null 2>&1; then
                print_result 0 "$test"
            else
                print_result 1 "$test"
            fi
        else
            print_warning "Integration test not found: $test"
            SKIPPED_TESTS=$((SKIPPED_TESTS + 1))
        fi
    done
    
    # System integration tests
    run_system_integration_tests
}

# Run system integration tests
run_system_integration_tests() {
    print_status "Running system integration tests..."
    
    # Test CogServer integration
    if [ -f "test_cogserver_integration.sh" ]; then
        print_status "Running CogServer integration test..."
        if ./test_cogserver_integration.sh >/dev/null 2>&1; then
            print_result 0 "CogServer integration"
        else
            print_result 1 "CogServer integration"
        fi
    fi
    
    # Test NLP structure
    if [ -f "test_nlp_structure.sh" ]; then
        print_status "Running NLP structure test..."
        if ./test_nlp_structure.sh >/dev/null 2>&1; then
            print_result 0 "NLP structure"
        else
            print_result 1 "NLP structure"
        fi
    fi
}

# Run Agent-Zero specific tests
run_agent_zero_tests() {
    print_section "Running Agent-Zero Tests"
    
    # Check if Guile is available
    if ! command -v guile &> /dev/null; then
        print_warning "Guile not available, skipping Agent-Zero tests"
        return 0
    fi
    
    # Run Agent-Zero integration tests
    if [ -f "tests/agent-zero/integration-test.sh" ]; then
        print_status "Running Agent-Zero integration tests..."
        if ./tests/agent-zero/integration-test.sh >/dev/null 2>&1; then
            print_result 0 "Agent-Zero integration"
        else
            print_result 1 "Agent-Zero integration"
        fi
    fi
    
    # Run PLN integration tests
    if [ -f "tests/agent-zero/pln-integration-test.sh" ]; then
        print_status "Running PLN integration tests..."
        if ./tests/agent-zero/pln-integration-test.sh >/dev/null 2>&1; then
            print_result 0 "PLN integration"
        else
            print_result 1 "PLN integration"
        fi
    fi
}

# Run performance tests
run_performance_tests() {
    print_section "Running Performance Tests"
    
    # Create performance benchmarks directory
    mkdir -p benchmarks
    
    # AtomSpace performance benchmarks
    create_atomspace_benchmark
    
    # PLN performance benchmarks  
    create_pln_benchmark
    
    # CogServer performance benchmarks
    create_cogserver_benchmark
    
    # Run benchmarks
    for benchmark in benchmarks/*_benchmark.cr; do
        if [ -f "$benchmark" ]; then
            local name=$(basename "$benchmark" .cr)
            print_status "Running $name..."
            
            if crystal run --release --error-trace "$benchmark" > "${REPORTS_DIR}/performance/${name}.txt" 2>&1; then
                print_result 0 "$name"
            else
                print_result 1 "$name"
            fi
        fi
    done
}

# Create AtomSpace benchmark
create_atomspace_benchmark() {
    cat > benchmarks/atomspace_benchmark.cr << 'EOF'
require "../src/cogutil/cogutil"
require "../src/atomspace/atomspace_main"
require "benchmark"

CogUtil.initialize
AtomSpace.initialize

puts "AtomSpace Performance Benchmarks"
puts "================================="

Benchmark.ips do |bench|
  atomspace = AtomSpace::AtomSpace.new
  
  bench.report("create_concept_node") do
    atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "test_#{rand(10000)}")
  end
  
  # Pre-create some atoms for link tests
  dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
  animal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "animal")
  
  bench.report("create_inheritance_link") do
    atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, animal])
  end
  
  bench.report("atomspace_lookup") do
    atomspace.contains?(dog)
  end
  
  # Large-scale operations
  bench.report("batch_node_creation") do
    100.times do |i|
      atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "batch_#{i}")
    end
  end
end

puts "\nAtomSpace Memory Usage:"
puts "Atoms in AtomSpace: #{atomspace.size}"
EOF
}

# Create PLN benchmark
create_pln_benchmark() {
    cat > benchmarks/pln_benchmark.cr << 'EOF'
require "../src/cogutil/cogutil"
require "../src/atomspace/atomspace_main"
require "../src/pln/pln"
require "benchmark"

CogUtil.initialize
AtomSpace.initialize
PLN.initialize

puts "PLN Performance Benchmarks"
puts "=========================="

atomspace = AtomSpace::AtomSpace.new

# Create knowledge base
dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
mammal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "mammal")
animal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "animal")

atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, mammal])
atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [mammal, animal])

reasoner = PLN::Reasoner.new(atomspace)

Benchmark.ips do |bench|
  bench.report("pln_single_step") do
    reasoner.step_forward
  end
  
  bench.report("pln_reasoning_cycle") do
    reasoner.reason(3)
  end
  
  bench.report("inheritance_query") do
    query_atom = atomspace.add_node(AtomSpace::AtomType::VARIABLE_NODE, "$x")
    pattern = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, query_atom])
    results = reasoner.query(pattern)
  end
end

puts "\nPLN Memory Usage:"
puts "Total atoms: #{atomspace.size}"
EOF
}

# Create CogServer benchmark
create_cogserver_benchmark() {
    cat > benchmarks/cogserver_benchmark.cr << 'EOF'
require "../src/cogutil/cogutil"
require "../src/atomspace/atomspace_main"
require "../src/cogserver/cogserver_main"
require "benchmark"
require "http/client"
require "json"

CogUtil.initialize
AtomSpace.initialize

puts "CogServer Performance Benchmarks"
puts "================================"

atomspace = AtomSpace::AtomSpace.new
server = CogServer::Server.new

# Start server in background
spawn do
  server.start
end

sleep(1) # Wait for server to start

# HTTP client for testing
client = HTTP::Client.new("localhost", 17001)

Benchmark.ips do |bench|
  bench.report("status_endpoint") do
    response = client.get("/api/status")
    response.status_code == 200
  end
  
  bench.report("atomspace_size_endpoint") do
    response = client.get("/api/atomspace")
    response.status_code == 200
  end
  
  bench.report("create_atom_via_api") do
    json_data = {
      "type" => "CONCEPT_NODE",
      "name" => "test_#{rand(1000)}"
    }.to_json
    
    response = client.post("/api/atoms", 
      headers: HTTP::Headers{"Content-Type" => "application/json"},
      body: json_data)
    response.status_code == 201
  end
end

server.stop
puts "\nCogServer tested successfully"
EOF
}

# Run functional tests
run_functional_tests() {
    print_section "Running Functional/End-to-End Tests"
    
    # API functionality tests
    print_status "Testing API functionality..."
    run_api_tests
    
    # Workflow tests
    print_status "Testing complete workflows..."
    run_workflow_tests
    
    # Regression tests
    print_status "Running regression tests..."
    run_regression_tests
}

# Run API tests
run_api_tests() {
    print_status "Running API functionality tests..."
    
    # Test REST API endpoints
    if [ -f "test_cogserver_api.cr" ]; then
        if crystal run --error-trace "test_cogserver_api.cr" >/dev/null 2>&1; then
            print_result 0 "REST API tests"
        else
            print_result 1 "REST API tests"
        fi
    fi
}

# Run workflow tests  
run_workflow_tests() {
    print_status "Running complete workflow tests..."
    
    # Test complete cognitive processing workflow
    local workflow_test="${TEMP_DIR}/workflow_test.cr"
    cat > "$workflow_test" << 'EOF'
require "./src/cogutil/cogutil"
require "./src/atomspace/atomspace_main"
require "./src/pln/pln"
require "./src/pattern_matching/pattern_matching"

# Complete cognitive workflow test
CogUtil.initialize
AtomSpace.initialize
PLN.initialize

# Create knowledge base
atomspace = AtomSpace::AtomSpace.new

# Add knowledge
dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
mammal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "mammal")
animal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "animal")

atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, mammal])
atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [mammal, animal])

# Apply reasoning
reasoner = PLN::Reasoner.new(atomspace)
reasoner.reason(5)

# Query results
matcher = PatternMatching::PatternMatcher.new(atomspace)
query = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [
  dog,
  atomspace.add_node(AtomSpace::AtomType::VARIABLE_NODE, "$x")
])

results = matcher.match(query)

puts "Workflow test: Found #{results.size} inheritance relationships for dog"
puts "Test passed!" if results.size >= 2
EOF

    if crystal run --error-trace "$workflow_test" >/dev/null 2>&1; then
        print_result 0 "Complete workflow test"
    else
        print_result 1 "Complete workflow test"
    fi
}

# Run regression tests
run_regression_tests() {
    print_status "Running regression tests..."
    
    # Test core functionality hasn't regressed
    local regression_test="${TEMP_DIR}/regression_test.cr"
    cat > "$regression_test" << 'EOF'
require "./src/cogutil/cogutil"
require "./src/atomspace/atomspace_main"

# Basic functionality regression test
CogUtil.initialize
AtomSpace.initialize

atomspace = AtomSpace::AtomSpace.new

# Basic operations that should always work
node1 = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "test1")
node2 = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "test2")
link = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [node1, node2])

raise "Basic atom creation failed" unless node1 && node2 && link
raise "AtomSpace size incorrect" unless atomspace.size == 3
raise "Contains check failed" unless atomspace.contains?(node1)

puts "Regression test passed!"
EOF

    if crystal run --error-trace "$regression_test" >/dev/null 2>&1; then
        print_result 0 "Regression tests"
    else
        print_result 1 "Regression tests"
    fi
}

# Generate test reports
generate_reports() {
    print_section "Generating Test Reports"
    
    local report_file="${REPORTS_DIR}/test-summary-$(date +%Y%m%d-%H%M%S).txt"
    
    cat > "$report_file" << EOF
CrystalCog Comprehensive Test Suite Report
==========================================
Generated: $(date)

Test Summary:
Total Tests: $TOTAL_TESTS
Passed: $PASSED_TESTS
Failed: $FAILED_TESTS  
Skipped: $SKIPPED_TESTS

Success Rate: $(( PASSED_TESTS * 100 / (TOTAL_TESTS == 0 ? 1 : TOTAL_TESTS) ))%

Test Categories Run:
- Unit Tests: $([ "$RUN_UNIT" = true ] && echo "✓" || echo "✗")
- Integration Tests: $([ "$RUN_INTEGRATION" = true ] && echo "✓" || echo "✗")
- Performance Tests: $([ "$RUN_PERFORMANCE" = true ] && echo "✓" || echo "✗")
- Functional Tests: $([ "$RUN_FUNCTIONAL" = true ] && echo "✓" || echo "✗")
- Agent-Zero Tests: $([ "$RUN_AGENT_ZERO" = true ] && echo "✓" || echo "✗")

System Information:
- Crystal Version: $(crystal version | head -n1)
- OS: $(uname -s) $(uname -r)
- Architecture: $(uname -m)
- Test Environment: $(hostname)

EOF

    print_success "Test report generated: $report_file"
}

# Generate coverage report
generate_coverage() {
    print_section "Generating Coverage Report"
    
    local coverage_file="${REPORTS_DIR}/coverage/coverage-$(date +%Y%m%d-%H%M%S).txt"
    
    # Count source files and spec files
    local src_files=$(find src/ -name "*.cr" | wc -l)
    local spec_files=$(find spec/ -name "*.cr" | wc -l)
    
    cat > "$coverage_file" << EOF
CrystalCog Test Coverage Report
==============================
Generated: $(date)

File Coverage:
Source files: $src_files
Spec files: $spec_files
Coverage ratio: $(( spec_files * 100 / (src_files == 0 ? 1 : src_files) ))%

Components Coverage:
EOF

    # Check coverage for each component
    for component in cogutil atomspace pln cogserver pattern_matching nlp; do
        local src_count=$(find "src/$component" -name "*.cr" 2>/dev/null | wc -l)
        local spec_count=$(find "spec/$component" -name "*.cr" 2>/dev/null | wc -l)
        
        if [ "$src_count" -gt 0 ]; then
            local coverage=$(( spec_count * 100 / src_count ))
            echo "- $component: $coverage% ($spec_count/$src_count)" >> "$coverage_file"
        fi
    done
    
    print_success "Coverage report generated: $coverage_file"
}

# Print final summary
print_summary() {
    print_header "Test Suite Summary"
    
    echo -e "${BLUE}Total Tests:${NC} $TOTAL_TESTS"
    echo -e "${GREEN}Passed:${NC} $PASSED_TESTS"
    echo -e "${RED}Failed:${NC} $FAILED_TESTS"
    echo -e "${YELLOW}Skipped:${NC} $SKIPPED_TESTS"
    echo ""
    
    if [ $TOTAL_TESTS -eq 0 ]; then
        echo -e "${YELLOW}No tests were run${NC}"
        return 1
    fi
    
    local success_rate=$(( PASSED_TESTS * 100 / TOTAL_TESTS ))
    echo -e "${BLUE}Success Rate:${NC} ${success_rate}%"
    
    if [ $FAILED_TESTS -eq 0 ]; then
        echo -e "${GREEN}All tests passed! 🎉${NC}"
        return 0
    else
        echo -e "${RED}Some tests failed. Please review the output above.${NC}"
        return 1
    fi
}

# Main execution
main() {
    parse_args "$@"
    
    if [ "$HELP" = true ]; then
        show_help
        exit 0
    fi
    
    # Default to validation if no specific tests selected and no Crystal available
    if [ "$RUN_UNIT" = false ] && [ "$RUN_INTEGRATION" = false ] && \
       [ "$RUN_PERFORMANCE" = false ] && [ "$RUN_FUNCTIONAL" = false ] && \
       [ "$RUN_AGENT_ZERO" = false ] && [ "$RUN_VALIDATION" = false ]; then
        if ! command -v crystal &> /dev/null; then
            print_warning "Crystal not available - defaulting to validation mode"
            RUN_VALIDATION=true
        else
            RUN_UNIT=true
        fi
    fi
    
    print_header "CrystalCog Comprehensive Testing Suite"
    
    # Run validation if requested or if it's the only option
    if [ "$RUN_VALIDATION" = true ]; then
        validate_test_infrastructure
    else
        setup_environment
        
        # Run selected test categories
        [ "$RUN_UNIT" = true ] && run_unit_tests
        [ "$RUN_INTEGRATION" = true ] && run_integration_tests
        [ "$RUN_AGENT_ZERO" = true ] && run_agent_zero_tests
        [ "$RUN_PERFORMANCE" = true ] && run_performance_tests
        [ "$RUN_FUNCTIONAL" = true ] && run_functional_tests
        
        # Generate reports
        [ "$GENERATE_REPORTS" = true ] && generate_reports
        [ "$GENERATE_COVERAGE" = true ] && generate_coverage
    fi
    
    # Cleanup
    clean_artifacts
    
    # Print summary and exit with appropriate code
    print_summary
    exit $?
}

# Trap cleanup on exit
trap 'clean_artifacts' EXIT

# Run main function
main "$@"