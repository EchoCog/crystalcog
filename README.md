


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
- **Pattern Matching**: Advanced pattern matching and query engine with recursive composition, temporal analysis, machine learning, and statistical inference
- **Persistence**: Multiple storage backends (File, SQLite, Network)
- **Cognitive Kernels**: Agent-Zero Genesis cognitive processing units with hypergraph state persistence
- **Tensor Field Encoding**: Mathematical sequence generators for cognitive state representation

### Key Features

#### AtomSpace Persistence
- **RocksDB Storage**: High-performance key-value storage (0.9ms store, 0.5ms load)
- **PostgreSQL Storage**: Enterprise-grade database for production and multi-user access
- **SQLite Storage**: Relational database with indexing for medium datasets  
- **File Storage**: Human-readable Scheme format for small datasets and debugging
- **Network Storage**: Distributed AtomSpace access via CogServer
- **Multiple Storage**: Attach multiple backends for redundancy and performance
- **Hypergraph State Persistence**: Complete cognitive kernel state management

#### Cognitive Kernel System (Agent-Zero Genesis)
- **Cognitive Kernels**: Complete cognitive processing units with state persistence
- **Tensor Field Encoding**: Mathematical sequence generators (prime, fibonacci, harmonic, factorial, powers of two)
- **Attention Allocation**: Adaptive priority management across multiple kernels
- **Meta-Cognitive Processing**: Recursive self-description and meta-level tracking
- **Operation-Specific Encodings**: Specialized tensor configurations for reasoning, learning, memory, attention

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

# Save to high-performance RocksDB storage
rocksdb_storage = atomspace.create_rocksdb_storage("main", "knowledge.rocks")
rocksdb_storage.open
rocksdb_storage.store_atomspace(atomspace)

# Or use PostgreSQL for production/enterprise
postgres_storage = atomspace.create_postgres_storage("prod", "user:pass@localhost/opencog")

# Or traditional file storage for debugging
file_storage = atomspace.create_file_storage("debug", "knowledge.scm")
file_storage.open
file_storage.store_atomspace(atomspace)

# Create cognitive kernel with hypergraph state persistence
kernel = AtomSpace::CognitiveKernel.new([128, 64], 0.8, 1, "reasoning")
kernel.add_concept_node("agent-zero")

# Store complete cognitive state
hypergraph_storage = kernel.atomspace.create_hypergraph_storage("cognitive", "state.scm")
hypergraph_storage.open
kernel.store_hypergraph_state(hypergraph_storage)

# Generate tensor field encodings
prime_encoding = kernel.tensor_field_encoding("prime", include_attention: true)
hypergraph_encoding = kernel.hypergraph_tensor_encoding

# Save via REST API
curl -X POST http://localhost:18080/storage/save
```

## Production Deployment

CrystalCog includes comprehensive production deployment scripts for enterprise-ready environments:

### Automated Production Setup

```bash
# Run the production environment setup (requires root)
sudo ./scripts/production/setup-production.sh

# Or with custom configuration
sudo ./scripts/production/setup-production.sh \
  --install-dir /opt/crystalcog \
  --service-user crystalcog \
  --backup-dir /backup/crystalcog
```

The production setup script automatically configures:
- **Docker & Docker Compose**: Containerized deployment
- **System Security**: UFW firewall, fail2ban intrusion detection
- **SSL Certificates**: Automated certificate management
- **Service Management**: Systemd service for auto-start
- **Monitoring Stack**: Prometheus, Grafana, and ELK stack
- **Backup System**: Automated backup cron jobs
- **Log Rotation**: Automatic log management

### Deployment Features

- **High Availability**: Multi-container architecture with health checks
- **Security Hardened**: Minimal attack surface, non-root containers
- **Monitoring Ready**: Complete observability stack included
- **Backup & Recovery**: Automated data protection
- **Scalable**: Resource limits and scaling configuration

### Validation & Testing

Validate your production setup:

```bash
# Comprehensive validation
./validate-setup-production.sh

# Docker Compose validation  
docker-compose -f docker-compose.production.yml config

# Health check
./scripts/production/healthcheck.sh
```

### Deployment Options

- **Container Deployment**: `docker-compose.production.yml`
- **Kubernetes Deployment**: `deployments/k8s/`
- **Guix System**: `guix environment -m guix.scm`
- **Manual Installation**: Traditional system installation

## Set up (Legacy Python/Rust Environment)
CrystalCog is a complete Crystal language implementation with all functionality.

## Documentation

For complete documentation:

- [Crystal Installation Guide](docs/CRYSTAL_INSTALLATION.md)
- [Development Roadmap](DEVELOPMENT-ROADMAP.md)
- [Persistence API Documentation](PERSISTENCE_API_DOCUMENTATION.md)
- [Hypergraph State Persistence Documentation](HYPERGRAPH_STATE_PERSISTENCE_DOCUMENTATION.md)
- [Advanced Pattern Matching Documentation](docs/ADVANCED_PATTERN_MATCHING.md)
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
