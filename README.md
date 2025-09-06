


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

### Testing

#### CogServer Integration Test

The CogServer includes a comprehensive integration test that validates all network API functionality:

```bash
# Build the CogServer
crystal build src/cogserver/cogserver_main.cr -o cogserver_bin

# Start CogServer for testing
crystal run start_test_cogserver.cr &

# Run integration test script
./test_cogserver_integration.sh
```

The integration test validates:
- HTTP REST API endpoints (7 endpoints)
- Telnet command interface (4 commands)
- WebSocket protocol upgrade
- Atom CRUD operations
- Error handling and validation

#### Full Test Suite

```bash
# Run all Crystal tests
crystal spec

# Run individual component tests
crystal run test_cogserver_api.cr
crystal run test_enhanced_api.cr
```

## Components

CrystalCog implements the complete OpenCog stack:

- **CogUtil**: Core utilities and logging
- **AtomSpace**: Hypergraph knowledge representation with comprehensive persistence
- **PLN**: Probabilistic Logic Networks for reasoning
- **URE**: Unified Rule Engine for inference
- **CogServer**: Network server for distributed processing with REST API
- **Pattern Matching**: Advanced pattern matching and query engine
- **Persistence**: Multiple storage backends (File, SQLite, Network)

### Key Features

#### AtomSpace Persistence
- **File Storage**: Human-readable Scheme format for small datasets
- **SQLite Storage**: Relational database with indexing for large datasets  
- **Network Storage**: Distributed AtomSpace access via CogServer
- **Multiple Storage**: Attach multiple backends for redundancy

#### Enhanced Network API
- **REST Endpoints**: Complete HTTP API for AtomSpace operations
- **Storage Management**: Attach/detach storage via REST API
- **WebSocket Support**: Real-time communication capabilities
- **Session Management**: Track client connections and state

#### Example Usage
```crystal
# Create AtomSpace with persistence
atomspace = AtomSpace::AtomSpace.new

# Add some knowledge
dog = atomspace.add_concept_node("dog")
animal = atomspace.add_concept_node("animal") 
atomspace.add_inheritance_link(dog, animal)

# Save to file storage
file_storage = atomspace.create_file_storage("main", "knowledge.scm")
atomspace.store_all

# Save via REST API
curl -X POST http://localhost:18080/storage/save
```

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
- [Persistence API Documentation](PERSISTENCE_API_DOCUMENTATION.md)
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
