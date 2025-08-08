# OpenCog Central Monorepo Makefile
# This Makefile provides convenient targets for building and managing the monorepo

# Configuration
BUILD_TYPE ?= Release
JOBS ?= $(shell nproc)
INSTALL_PREFIX ?= /usr/local
BUILD_DIR ?= build
SKIP_TESTS ?= false
SKIP_INSTALL ?= false
CLEAN_BUILD ?= false

# Colors for output
RED := \033[0;31m
GREEN := \033[0;32m
YELLOW := \033[1;33m
BLUE := \033[0;34m
NC := \033[0m # No Color

# Default target
.PHONY: all
all: build

# Help target
.PHONY: help
help:
	@echo "OpenCog Central Monorepo Makefile"
	@echo ""
	@echo "Available targets:"
	@echo "  all              - Build all components (default)"
	@echo "  build            - Build all components"
	@echo "  test             - Run all tests"
	@echo "  install          - Install all components"
	@echo "  clean            - Clean build directory"
	@echo "  setup            - Setup dependencies only"
	@echo "  dev-env          - Setup development environment"
	@echo "  core             - Build core components only"
	@echo "  extended         - Build extended components"
	@echo "  doc              - Generate documentation"
	@echo "  package          - Create package"
	@echo ""
	@echo "Agent-Zero Genesis targets:"
	@echo "  agent-zero       - Build Agent-Zero Genesis system"
	@echo "  agent-zero-setup - Setup Agent-Zero environment only"
	@echo "  agent-zero-test  - Test Agent-Zero components"
	@echo "  agent-zero-demo  - Run Agent-Zero demonstration"
	@echo "  agent-zero-clean - Clean Agent-Zero build artifacts"
	@echo ""
	@echo "Guix environment targets:"
	@echo "  guix-env         - Enter Guix development environment"
	@echo "  guix-shell       - Enter Guix containerized shell"
	@echo ""
	@echo "  help             - Show this help message"
	@echo ""
	@echo "Environment variables:"
	@echo "  BUILD_TYPE       - Build type (Debug, Release, RelWithDebInfo) [default: Release]"
	@echo "  JOBS             - Number of parallel jobs [default: \$(nproc)]"
	@echo "  INSTALL_PREFIX   - Installation prefix [default: /usr/local]"
	@echo "  BUILD_DIR        - Build directory [default: build]"
	@echo "  SKIP_TESTS       - Skip tests (true/false) [default: false]"
	@echo "  SKIP_INSTALL     - Skip installation (true/false) [default: false]"
	@echo "  CLEAN_BUILD      - Clean build directory (true/false) [default: false]"
	@echo ""
	@echo "Examples:"
	@echo "  make                    # Build with default settings"
	@echo "  make BUILD_TYPE=Debug   # Debug build"
	@echo "  make JOBS=4            # Build with 4 jobs"
	@echo "  make SKIP_TESTS=true   # Build without tests"
	@echo "  make clean build       # Clean build"
	@echo "  make agent-zero        # Build Agent-Zero Genesis"
	@echo "  make guix-env          # Enter Guix development environment"

# Setup dependencies
.PHONY: setup
setup:
	@echo "$(BLUE)[INFO]$(NC) Setting up dependencies..."
	@./scripts/build-monorepo.sh --setup-only

# Setup development environment
.PHONY: dev-env
dev-env:
	@echo "$(BLUE)[INFO]$(NC) Setting up development environment..."
	@./scripts/build-monorepo.sh --dev-env

# Configure CMake
.PHONY: configure
configure:
	@echo "$(BLUE)[INFO]$(NC) Configuring CMake..."
	@mkdir -p $(BUILD_DIR)
	@cd $(BUILD_DIR) && cmake .. \
		-DCMAKE_BUILD_TYPE=$(BUILD_TYPE) \
		-DCMAKE_INSTALL_PREFIX=$(INSTALL_PREFIX) \
		-DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
		-DBUILD_SHARED_LIBS=ON \
		-DCMAKE_CXX_STANDARD=17 \
		-DCMAKE_CXX_STANDARD_REQUIRED=ON

# Build all components
.PHONY: build
build: configure
	@echo "$(BLUE)[INFO]$(NC) Building all components..."
	@cd $(BUILD_DIR) && make -j$(JOBS)
	@echo "$(GREEN)[SUCCESS]$(NC) Build complete"

# Build core components only
.PHONY: core
core: configure
	@echo "$(BLUE)[INFO]$(NC) Building core components..."
	@cd $(BUILD_DIR) && make cogutil atomspace attention ure pln link-grammar cogserver -j$(JOBS)
	@echo "$(GREEN)[SUCCESS]$(NC) Core build complete"

# Build extended components
.PHONY: extended
extended: core
	@echo "$(BLUE)[INFO]$(NC) Building extended components..."
	@cd $(BUILD_DIR) && make -j$(JOBS)
	@echo "$(GREEN)[SUCCESS]$(NC) Extended build complete"

# Run tests
.PHONY: test
test: build
ifeq ($(SKIP_TESTS),true)
	@echo "$(YELLOW)[WARNING]$(NC) Skipping tests as requested"
else
	@echo "$(BLUE)[INFO]$(NC) Running tests..."
	@cd $(BUILD_DIR) && ctest --output-on-failure -j$(JOBS)
	@echo "$(GREEN)[SUCCESS]$(NC) Tests complete"
endif

# Install components
.PHONY: install
install: build
ifeq ($(SKIP_INSTALL),true)
	@echo "$(YELLOW)[WARNING]$(NC) Skipping installation as requested"
else
	@echo "$(BLUE)[INFO]$(NC) Installing components..."
	@cd $(BUILD_DIR) && make install
	@echo "$(GREEN)[SUCCESS]$(NC) Installation complete"
	@echo "$(BLUE)[INFO]$(NC) You may need to run: sudo ldconfig"
endif

# Generate documentation
.PHONY: doc
doc: build
	@echo "$(BLUE)[INFO]$(NC) Generating documentation..."
	@cd $(BUILD_DIR) && make doc
	@echo "$(GREEN)[SUCCESS]$(NC) Documentation generated"

# Create package
.PHONY: package
package: build
	@echo "$(BLUE)[INFO]$(NC) Creating package..."
	@cd $(BUILD_DIR) && checkinstall --pkgname=opencog-central --pkgversion=1.0.0 --backup=no --fstrans=no --default
	@echo "$(GREEN)[SUCCESS]$(NC) Package created"

# Clean build directory
.PHONY: clean
clean:
	@echo "$(BLUE)[INFO]$(NC) Cleaning build directory..."
	@rm -rf $(BUILD_DIR)
	@echo "$(GREEN)[SUCCESS]$(NC) Build directory cleaned"

# Clean and rebuild
.PHONY: rebuild
rebuild: clean build

# Full build with tests and installation
.PHONY: full
full: setup build test install

# Quick build (skip tests and installation)
.PHONY: quick
quick: build

# Development build
.PHONY: dev
dev: BUILD_TYPE=Debug
dev: build test

# Release build
.PHONY: release
release: BUILD_TYPE=Release
release: clean build test install

# Profile build
.PHONY: profile
profile: BUILD_TYPE=RelWithDebInfo
profile: build

# Coverage build
.PHONY: coverage
coverage: BUILD_TYPE=Coverage
coverage: build test

# Install system dependencies
.PHONY: deps
deps:
	@echo "$(BLUE)[INFO]$(NC) Installing system dependencies..."
	@./scripts/build-monorepo.sh --setup-only

# Setup Python environment
.PHONY: python-env
python-env:
	@echo "$(BLUE)[INFO]$(NC) Setting up Python environment..."
	@python3 -m venv venv
	@source venv/bin/activate && pip install --upgrade pip
	@source venv/bin/activate && pip install -r requirements.txt
	@echo "$(GREEN)[SUCCESS]$(NC) Python environment setup complete"

# Setup Node.js environment
.PHONY: node-env
node-env:
	@echo "$(BLUE)[INFO]$(NC) Setting up Node.js environment..."
	@if [ -f "package.json" ]; then npm install; fi
	@echo "$(GREEN)[SUCCESS]$(NC) Node.js environment setup complete"

# Setup Rust environment
.PHONY: rust-env
rust-env:
	@echo "$(BLUE)[INFO]$(NC) Setting up Rust environment..."
	@if [ -f "Cargo.toml" ]; then cargo build --release; fi
	@echo "$(GREEN)[SUCCESS]$(NC) Rust environment setup complete"

# Run specific component tests
.PHONY: test-%
test-%: build
	@echo "$(BLUE)[INFO]$(NC) Running tests for $*..."
	@cd $(BUILD_DIR) && ctest --output-on-failure -R $*

# Build specific component
.PHONY: build-%
build-%: configure
	@echo "$(BLUE)[INFO]$(NC) Building $*..."
	@cd $(BUILD_DIR) && make $* -j$(JOBS)

# Install specific component
.PHONY: install-%
install-%: build-%
	@echo "$(BLUE)[INFO]$(NC) Installing $*..."
	@cd $(BUILD_DIR) && make install

# Show build status
.PHONY: status
status:
	@echo "$(BLUE)[INFO]$(NC) Build Status:"
	@echo "  Build Type: $(BUILD_TYPE)"
	@echo "  Jobs: $(JOBS)"
	@echo "  Install Prefix: $(INSTALL_PREFIX)"
	@echo "  Build Directory: $(BUILD_DIR)"
	@echo "  Skip Tests: $(SKIP_TESTS)"
	@echo "  Skip Install: $(SKIP_INSTALL)"
	@echo "  Clean Build: $(CLEAN_BUILD)"
	@if [ -d "$(BUILD_DIR)" ]; then \
		echo "  Build Directory: $(GREEN)Exists$(NC)"; \
	else \
		echo "  Build Directory: $(RED)Missing$(NC)"; \
	fi

# Show component list
.PHONY: components
components:
	@echo "$(BLUE)[INFO]$(NC) Available components:"
	@echo "Core Components:"
	@echo "  cogutil atomspace attention ure pln link-grammar cogserver"
	@echo ""
	@echo "Extended Components:"
	@echo "  asmoses agents agi-bio atomspace-agents atomspace-bridge"
	@echo "  atomspace-cog atomspace-dht atomspace-explorer atomspace-ipfs"
	@echo "  atomspace-js atomspace-metta atomspace-restful atomspace-rocks"
	@echo "  atomspace-rpc atomspace-typescript atomspace-websockets"
	@echo "  benchmark blender_api blender_api_msgs cheminformatics"
	@echo "  cogprotolab destin dimensional-embedding distributional-value"
	@echo "  external-tools generate ghost_bridge guile-dbi"
	@echo "  language-learning learn lg-atomese logicmoo_cogserver"
	@echo "  loving-ai loving-ai-ghost miner moses ocpkg opencog"
	@echo "  opencog-cycl opencog-debian opencog-neo4j opencog-nix"
	@echo "  opencog.org opencog_rpi opencog-to-minecraft pattern-index"
	@echo "  pau2motors perception pi_vision pln-brca-xp python-attic"
	@echo "  python-client python-destin relex rest-api-documentation"
	@echo "  robots_config rocca ros-behavior-scripting"
	@echo "  ros_opencog_robot_embodiment semantic-vision sensory"
	@echo "  spacetime stochastic-language-generation test-datasets"
	@echo "  TinyCog tv-toolbox unify unity3d-opencog-game"
	@echo "  vision visualization"

# Show help for specific component
.PHONY: help-%
help-%:
	@echo "$(BLUE)[INFO]$(NC) Help for component: $*"
	@echo "Available targets:"
	@echo "  build-$*     - Build $*"
	@echo "  test-$*      - Test $*"
	@echo "  install-$*   - Install $*"
	@echo ""
	@echo "Example: make build-$*"

# Agent-Zero Genesis targets
.PHONY: agent-zero agent-zero-setup agent-zero-test agent-zero-clean agent-zero-demo

# Build Agent-Zero Genesis (standalone)
agent-zero:
	@echo "$(BLUE)[INFO]$(NC) Building Agent-Zero Genesis..."
	@./scripts/agent-zero/build-agent-zero.sh
	@echo "$(GREEN)[SUCCESS]$(NC) Agent-Zero Genesis build complete"

# Setup Agent-Zero environment only
agent-zero-setup:
	@echo "$(BLUE)[INFO]$(NC) Setting up Agent-Zero environment..."
	@./scripts/agent-zero/build-agent-zero.sh --setup-only
	@echo "$(GREEN)[SUCCESS]$(NC) Agent-Zero environment setup complete"

# Test Agent-Zero components
agent-zero-test: agent-zero
	@echo "$(BLUE)[INFO]$(NC) Testing Agent-Zero Genesis..."
	@./tests/agent-zero/integration-test.sh
	@echo "$(GREEN)[SUCCESS]$(NC) Agent-Zero tests complete"

# Run Agent-Zero demonstration
agent-zero-demo: agent-zero
	@echo "$(BLUE)[INFO]$(NC) Running Agent-Zero Genesis demonstration..."
	@./scripts/agent-zero/demo-agent-zero.sh
	@echo "$(GREEN)[SUCCESS]$(NC) Agent-Zero demonstration complete"

# Clean Agent-Zero build artifacts
agent-zero-clean:
	@echo "$(BLUE)[INFO]$(NC) Cleaning Agent-Zero build artifacts..."
	@rm -rf build/agent-zero
	@echo "$(GREEN)[SUCCESS]$(NC) Agent-Zero artifacts cleaned"

# Guix environment setup
.PHONY: guix-env guix-shell

# Enter Guix development environment
guix-env:
	@echo "$(BLUE)[INFO]$(NC) Entering Guix development environment..."
	@echo "$(YELLOW)[NOTE]$(NC) This will start a new shell with Agent-Zero dependencies"
	@guix environment -m guix.scm

# Enter Guix shell (containerized)
guix-shell:
	@echo "$(BLUE)[INFO]$(NC) Entering Guix containerized shell..."
	@echo "$(YELLOW)[NOTE]$(NC) This will start a containerized environment"
	@guix shell -m guix.scm --container --pure