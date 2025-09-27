# Comprehensive Testing Suite for CrystalCog

This directory contains the comprehensive testing suite for the CrystalCog project, implementing the Agent-Zero Genesis roadmap testing requirements.

## Structure

```
tests/
├── README.md                    # This file
├── comprehensive-test-suite.sh  # Main test suite runner
├── unit/                        # Unit tests  
│   ├── cogutil/                # CogUtil unit tests
│   ├── atomspace/              # AtomSpace unit tests
│   ├── pln/                    # PLN unit tests
│   ├── cogserver/              # CogServer unit tests
│   └── pattern_matching/       # Pattern matching tests
├── integration/                 # Integration tests
│   ├── agent-zero/             # Agent-Zero integration tests
│   ├── crystal-tests/          # Crystal component integration
│   └── system/                 # Full system integration
├── performance/                 # Performance/benchmark tests
│   ├── atomspace-benchmarks/   # AtomSpace performance tests
│   ├── pln-benchmarks/         # PLN reasoning benchmarks
│   └── cogserver-benchmarks/   # CogServer performance tests
├── functional/                  # Functional/end-to-end tests
│   ├── api-tests/              # API functionality tests  
│   ├── workflow-tests/         # Complete workflow tests
│   └── regression-tests/       # Regression test suite
└── reports/                     # Test reports and coverage
    ├── coverage/               # Test coverage reports
    ├── performance/            # Performance test results
    └── integration/            # Integration test results
```

## Test Categories

### 1. Unit Tests
- **CogUtil**: Configuration, logging, utilities
- **AtomSpace**: Atom creation, storage, retrieval
- **PLN**: Reasoning rules, inference engines
- **CogServer**: API endpoints, session management
- **Pattern Matching**: Query language, pattern recognition

### 2. Integration Tests  
- **Agent-Zero**: Cognitive kernel tests, meta-cognition
- **Crystal Components**: Cross-component interactions
- **System Integration**: Full system workflow tests

### 3. Performance Tests
- **AtomSpace Benchmarks**: Storage and retrieval performance
- **PLN Benchmarks**: Reasoning performance and accuracy
- **CogServer Benchmarks**: API response times and throughput

### 4. Functional Tests
- **API Tests**: REST API functionality and validation
- **Workflow Tests**: Complete cognitive processing workflows
- **Regression Tests**: Prevent regressions in core functionality

## Usage

### Run All Tests
```bash
./tests/comprehensive-test-suite.sh --all
```

### Run Specific Test Category
```bash
./tests/comprehensive-test-suite.sh --unit
./tests/comprehensive-test-suite.sh --integration  
./tests/comprehensive-test-suite.sh --performance
./tests/comprehensive-test-suite.sh --functional
```

### Run Tests for Specific Component
```bash
./tests/comprehensive-test-suite.sh --component cogutil
./tests/comprehensive-test-suite.sh --component atomspace
./tests/comprehensive-test-suite.sh --component pln
```

### Generate Reports
```bash
./tests/comprehensive-test-suite.sh --reports
./tests/comprehensive-test-suite.sh --coverage
```

## Test Standards

### Crystal Tests
- Use Crystal's built-in spec framework
- Follow naming convention: `*_spec.cr`
- Include setup/teardown for resources
- Mock external dependencies

### Shell Script Tests
- Use assert functions for validation
- Include descriptive test names
- Clean up resources after tests
- Return proper exit codes

### Agent-Zero Tests
- Use Guile test framework when available
- Test cognitive kernel functionality
- Validate meta-cognitive processes
- Test attention allocation

## Coverage Goals

- **Unit Tests**: 90%+ code coverage
- **Integration Tests**: All major workflows covered
- **Performance Tests**: Baseline benchmarks established
- **Functional Tests**: All user-facing features tested

## Continuous Integration

The comprehensive test suite integrates with:
- GitHub Actions for CI/CD
- Local development workflows
- Docker-based testing environments
- Performance regression detection

## Contributing

When adding new features:
1. Add corresponding unit tests
2. Update integration tests if needed
3. Add performance benchmarks for critical code
4. Update documentation and test coverage reports