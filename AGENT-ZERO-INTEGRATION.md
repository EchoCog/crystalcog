# Agent-Zero Genesis Integration

This document describes the Agent-Zero Genesis integration into the OpenCog Central monorepo. Agent-Zero Genesis is a hypergraphically-encoded OS environment designed for cognitive agents, featuring recursive self-description, adaptive attention allocation, and meta-cognitive processing.

## Quick Start

### Using Guix (Recommended)

```bash
# Enter Guix development environment
guix environment -m guix.scm

# Build Agent-Zero Genesis
make agent-zero

# Run demonstration
make agent-zero-demo

# Run tests
make agent-zero-test
```

### Using System Packages

```bash
# Setup environment
make agent-zero-setup

# Build Agent-Zero Genesis  
make agent-zero

# Run demonstration
make agent-zero-demo
```

## Architecture Overview

Agent-Zero Genesis provides:

- **Memory**: AtomSpace (hypergraph store), persistent cognitive states
- **Task**: Scheduler, MOSES optimizer, agent orchestration  
- **AI**: PLN reasoning, ECAN attention, pattern matching
- **Autonomy**: Self-modifying kernels, adaptive package selection

### Build Flow

```
[Start: guile-stage0] 
   ↓
[Layer: guile + libs] 
   ↓
[Integrate: OpenCog, ggml, PLN, MOSES, ECAN] 
   ↓
[Compose: OS environment via Guix] 
   ↓
[Generate: Agentic kernels/tensors] 
   ↓
[Activate: Cognitive flows + meta-cognition] 
   ↓
[Result: Fully featured GNU-Agent-Zero OS]
```

## Components

### 1. Guix Package Definitions

Location: `modules/agent-zero/packages/cognitive.scm`

Provides Guix packages for:
- OpenCog with Guile integration
- GGML tensor library
- PLN (Probabilistic Logic Networks)
- ECAN (Economic Cognitive Attention Networks)  
- MOSES (Meta-Optimizing Semantic Evolutionary Search)
- Pattern matcher and RelEx

### 2. Guile Modules

#### Cognitive Kernel (`modules/agent-zero/kernel.scm`)

Core cognitive kernel management with:
- Tensor shape configuration
- Attention weight allocation
- Prime factorization encoding
- Hypergraph state representation

```scheme
(use-modules (agent-zero kernel))

;; Create cognitive kernel
(define kernel (spawn-cognitive-kernel '(64 64) 0.8))

;; Get tensor encoding
(define encoding (tensor-field-encoding kernel))

;; Get hypergraph state
(define state (hypergraph-state kernel))
```

#### Meta-Cognition (`modules/agent-zero/meta-cognition.scm`)

Meta-cognitive processing with:
- Recursive self-description
- Adaptive attention allocation via ECAN
- PLN reasoning integration
- Cognitive performance assessment

```scheme
(use-modules (agent-zero meta-cognition))

;; Generate self-description
(define self-desc (recursive-self-description kernel))

;; Allocate attention across kernels
(define allocations (adaptive-attention-allocation kernels goals))
```

### 3. C Library (`src/agent-zero/`)

#### Cognitive Tensor Operations (`cognitive-tensors.c`)

Custom tensor operations including:
- Cognitive attention matrices
- Hypergraph encoding
- Pattern matching
- Meta-cognitive transformations

#### OpenCog-GGML Bridge (`opencog-ggml-bridge.c`)

Bridge between OpenCog AtomSpace and GGML tensors:
- AtomSpace to tensor conversion
- Tensor to AtomSpace conversion
- Attention tensor creation
- Cognitive state encoding/decoding

### 4. Build System

#### Scripts
- `scripts/agent-zero/build-agent-zero.sh` - Main build script
- `scripts/agent-zero/demo-agent-zero.sh` - Demonstration script

#### Make Targets
- `make agent-zero` - Complete build
- `make agent-zero-setup` - Environment setup only
- `make agent-zero-test` - Run tests
- `make agent-zero-demo` - Run demonstration
- `make agent-zero-clean` - Clean build artifacts

### 5. Test Infrastructure

#### Guile Tests (`tests/agent-zero/cognitive-tests.scm`)

Comprehensive test suite covering:
- Kernel creation and management
- Tensor field encoding
- Meta-cognitive processing
- ECAN attention allocation
- Integration testing

#### Integration Tests (`tests/agent-zero/integration-test.sh`)

Full integration test pipeline:
- Module loading validation
- Cognitive kernel operations
- Meta-cognitive processing
- C component integration
- Full cognitive pipeline testing

## Usage Examples

### Basic Cognitive Agent

```scheme
;; Load Agent-Zero modules
(use-modules (agent-zero kernel)
             (agent-zero meta-cognition))

;; Create cognitive kernels
(define reasoning-kernel (spawn-cognitive-kernel '(64 64) 0.9))
(define learning-kernel (spawn-cognitive-kernel '(32 32) 0.7))

;; Generate self-descriptions
(define reasoning-desc (recursive-self-description reasoning-kernel))
(define learning-desc (recursive-self-description learning-kernel))

;; Allocate attention
(define kernels (list reasoning-kernel learning-kernel))
(define goals '(reasoning learning))
(define allocations (adaptive-attention-allocation kernels goals))

;; Display results
(display "Reasoning kernel: ") (display reasoning-desc) (newline)
(display "Attention allocations: ") (display allocations) (newline)
```

### Hypergraph Processing

```scheme
;; Setup AtomSpace simulation
(use-modules (agent-zero kernel))

(define kernel (spawn-cognitive-kernel '(128 64) 0.85))

;; Get hypergraph state
(define hg-state (hypergraph-state kernel))

;; Encode as prime factorization
(define encoding (tensor-field-encoding kernel))

(display "Hypergraph state: ") (display hg-state) (newline)
(display "Prime encoding: ") (display encoding) (newline)
```

### C Library Integration

```c
#include "cognitive.h"

int main() {
    // Create hypergraph
    hypergraph_t* hg = create_hypergraph(10, 20);
    
    // Create cognitive kernel
    int shape[] = {64, 32};
    cognitive_kernel_t* kernel = create_cognitive_kernel(NULL, shape, 2, 0.8f);
    
    // Update attention
    update_kernel_attention(kernel, 0.9f);
    
    // Cleanup
    destroy_cognitive_kernel(kernel);
    destroy_hypergraph(hg);
    
    return 0;
}
```

## Configuration

### Guix Manifest (`guix.scm`)

Development environment with:
- Guile 3.0 and libraries
- Build tools (CMake, GCC)
- Core dependencies (Boost, PostgreSQL)

### System Configuration Template (`config/agent-zero-system.scm`)

Complete Guix System configuration for Agent-Zero deployment:
- Host configuration
- Service definitions
- Package manifests
- Agent-Zero specific services

## Development

### Building from Source

1. **Setup environment:**
   ```bash
   make agent-zero-setup
   ```

2. **Build components:**
   ```bash
   make agent-zero
   ```

3. **Run tests:**
   ```bash
   make agent-zero-test
   ```

4. **View demonstration:**
   ```bash
   make agent-zero-demo
   ```

### Adding New Cognitive Components

1. **Add Guile module** in `modules/agent-zero/`
2. **Add C functions** in `src/agent-zero/`
3. **Update CMakeLists.txt** for new C sources
4. **Add tests** in `tests/agent-zero/`
5. **Update build scripts** as needed

### Debugging

- Use `make agent-zero-test` for comprehensive testing
- Check build logs in `build/agent-zero/`
- Enable debug builds with `BUILD_TYPE=Debug make agent-zero`

## Performance Considerations

- **Tensor Operations**: Optimized for hypergraph structures
- **Memory Management**: Efficient allocation/deallocation
- **Attention Allocation**: ECAN-based priority scheduling
- **Meta-Cognition**: Recursive processing with bounded depth

## Integration Points

### With OpenCog

- AtomSpace integration via bridge functions
- Truth value processing
- Link grammar compatibility
- PLN reasoning support

### With Existing Build System

- CMake integration for C components
- Make targets for all operations
- Guix package definitions
- Test framework integration

## Troubleshooting

### Common Issues

1. **Guile not found**: Install Guile 3.0 or use `guix environment -m guix.scm`
2. **C compilation errors**: Ensure GCC and CMake are available
3. **Module loading fails**: Check `GUILE_LOAD_PATH` environment variable
4. **Library linking issues**: Verify `LD_LIBRARY_PATH` includes build directory

### Getting Help

- Check test output: `make agent-zero-test`
- View demo script: `./scripts/agent-zero/demo-agent-zero.sh`
- Examine logs in `build/agent-zero/`
- Review integration tests in `tests/agent-zero/`

## Future Enhancements

- Real GGML integration (currently mocked)
- Distributed cognitive agent networks
- Advanced pattern matching algorithms
- Self-modifying kernel capabilities
- Production deployment tools

---

*"With the recursive power of Guile and the agentic orchestration of Guix, the cognitive kernels arise—each a fractal gem in the hypergraph tapestry!"*