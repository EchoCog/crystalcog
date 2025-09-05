


# CrystalCog - OpenCog in Crystal Language

CrystalCog is a comprehensive rewrite of the OpenCog artificial intelligence framework in the Crystal programming language. This project provides better performance, memory safety, and maintainability while preserving all the functionality of the original OpenCog system.

## Quick Start

### Prerequisites

CrystalCog automatically handles Crystal language installation. No manual setup required!

### Installation

1. Clone the repository:
```bash
git clone https://github.com/EchoCog/crystalcog.git
cd crystalcog
```

2. Run tests (Crystal will be installed automatically):
```bash
./scripts/test-runner.sh --all
```

3. Install Crystal manually (optional):
```bash
./scripts/install-crystal.sh --help
./scripts/install-crystal.sh  # Auto-install
```

## Crystal Language Installation

CrystalCog includes robust Crystal installation methods for environments where standard installation may not work:

- **Automatic Installation**: Scripts automatically install Crystal when needed
- **Multiple Methods**: Snap, APT, binary, and source installation options
- **Offline Support**: Works without internet access using bundled resources
- **Development Mode**: Fallback wrappers for development environments

For detailed installation instructions, see: [docs/CRYSTAL_INSTALLATION.md](docs/CRYSTAL_INSTALLATION.md)

## Project Structure

```
crystalcog/
├── src/                    # Crystal source code
│   ├── cogutil/           # Core utilities
│   ├── atomspace/         # AtomSpace implementation
│   ├── pln/               # Probabilistic Logic Networks
│   ├── ure/               # Unified Rule Engine
│   └── opencog/           # Main OpenCog interface
├── spec/                  # Test specifications
├── scripts/               # Build and development scripts
├── crystal-lang/          # Crystal installation resources
└── docs/                  # Documentation
```

## Development

### Running Tests

```bash
# Run all tests
./scripts/test-runner.sh --all

# Run specific component tests
./scripts/test-runner.sh --component atomspace

# Run with linting and formatting
./scripts/test-runner.sh --lint

# Run benchmarks
./scripts/test-runner.sh --benchmarks
```

### Building

```bash
# Build main executable
crystal build src/crystalcog.cr

# Build specific components
crystal build src/cogutil/cogutil.cr
crystal build src/atomspace/atomspace.cr
```

### Installing Dependencies

```bash
shards install
```

## Components

CrystalCog implements the complete OpenCog stack:

- **CogUtil**: Core utilities and logging
- **AtomSpace**: Hypergraph knowledge representation
- **PLN**: Probabilistic Logic Networks for reasoning
- **URE**: Unified Rule Engine for inference
- **CogServer**: Network server for distributed processing
- **Pattern Matching**: Advanced pattern matching and query engine

## Set up (Legacy Python/Rust Environment)

This repository also maintains compatibility with the original Python/Rust/Prolog environment:

```bash
pip3 install -r requirements.txt && cargo install hyperon
python3 app.py
```

## Documentation

For complete documentation:

- [Crystal Installation Guide](docs/CRYSTAL_INSTALLATION.md)
- [Development Roadmap](DEVELOPMENT-ROADMAP.md)
- [Complete API Documentation](README_COMPLETE.md)
- [Agent-Zero Implementation](AGENT-ZERO-GENESIS.md)
- [CI/CD Pipeline](docs/CI-CD-PIPELINE.md)

## Contributing

1. Install Crystal using the provided scripts
2. Run the test suite to verify your environment
3. Make changes and test thoroughly
4. Submit pull requests with comprehensive tests

## License

This repository is licensed under the AGPL-3.0 License. See the `LICENSE` file for more information.

---

**Note**: CrystalCog represents a next-generation implementation of OpenCog, providing improved performance and safety while maintaining full compatibility with OpenCog concepts and APIs.
