# CrystalCog Scripts

This directory contains utility scripts for the CrystalCog project.

## Available Scripts

### `test-runner.sh` - Comprehensive Test Runner
A comprehensive testing script that provides local development testing capabilities matching the CI/CD pipeline.

**Usage:**
```bash
./test-runner.sh [OPTIONS]

Options:
  -h, --help          Show help message
  -v, --verbose       Run tests with verbose output
  -c, --coverage      Generate coverage reports  
  -b, --benchmarks    Run performance benchmarks
  -i, --integration   Run integration tests
  -l, --lint          Run code linting and formatting checks
  -B, --build         Build all targets before testing
  -C, --component     Run tests for specific component
  -a, --all          Run all tests (comprehensive)
```

**Examples:**
```bash
# Run complete test suite
./test-runner.sh --all

# Run unit tests with linting
./test-runner.sh --lint --verbose

# Test specific component
./test-runner.sh --component atomspace

# Run benchmarks only
./test-runner.sh --benchmarks

# Build and test with integration tests
./test-runner.sh --build --integration
```

### `build-monorepo.sh` - Monorepo Build Script
Legacy build script for the monorepo structure (primarily C++ components).

### `demo-monorepo.sh` - Monorepo Demo Script  
Interactive demo script for the monorepo build system.

## CI/CD Integration

The test-runner.sh script is designed to mirror the CI/CD pipeline behavior locally:
- Same test execution order
- Same component organization
- Compatible output formats
- Matching quality gates

This allows developers to run the same tests locally that will run in the CI/CD pipeline.

## Script Permissions

Make sure scripts are executable:
```bash
chmod +x scripts/*.sh
```

## Adding New Scripts

When adding new utility scripts:
1. Make them executable (`chmod +x`)
2. Include appropriate error handling
3. Provide help/usage information
4. Update this README
5. Follow existing naming conventions