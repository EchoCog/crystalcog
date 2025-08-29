# OpenCog to Crystal Language Conversion Roadmap

## Project Overview

This document outlines the comprehensive plan to convert the entire OpenCog project from C++/Python/Scheme to Crystal language. The conversion will preserve all functionality while leveraging Crystal's performance, safety, and expressiveness.

## Repository Analysis Summary

- **Total Size**: ~2.3GB across 102 components
- **C++ Files**: 5,127 files (.cpp, .cc, .h, .hpp)
- **Python Files**: 732 files (.py)  
- **Scheme Files**: 1,801 files (.scm)
- **Components**: 102 main components analyzed
- **Core Components**: 10 foundation libraries identified
- **Language Distribution**: 70% C++, 20% Python, 10% Scheme

## Component Ranking Methodology

Each component is evaluated on three key metrics:

1. **Size Score** (0-100): Relative size as percentage of total codebase
2. **Complexity Score** (0-100): Difficulty of C++→Crystal conversion
3. **Priority Score** (0-100): Dependency criticality and core functionality importance

**Final Ranking Formula**: `(Priority × 0.5) + (Size × 0.3) + (100 - Complexity × 0.2)`

Higher scores indicate higher priority for conversion.

## Component Rankings

### Phase 1: Foundation (Scores 85-100)

| Component | Size | Size% | Complexity | Priority | Final Score | Dependencies |
|-----------|------|--------|------------|----------|-------------|--------------|
| **cogutil** | 1.6M | 0.07% | 25 | 100 | 95.0 | None (base) |
| **atomspace** | 18M | 0.78% | 85 | 100 | 93.4 | cogutil |
| **opencog** | 8.6M | 0.37% | 80 | 95 | 91.9 | atomspace, cogutil |

### Phase 2: Core Reasoning (Scores 75-90)

| Component | Size | Size% | Complexity | Priority | Final Score | Dependencies |
|-----------|------|--------|------------|----------|-------------|--------------|
| **pln** | 1.8M | 0.08% | 90 | 90 | 85.2 | atomspace, opencog |
| **ure** | 1.3M | 0.06% | 85 | 90 | 85.0 | atomspace, opencog |
| **cogserver** | 788K | 0.03% | 60 | 85 | 82.0 | atomspace, cogutil |
| **attention** | 1.1M | 0.05% | 70 | 80 | 77.0 | atomspace |
| **miner** | 1.1M | 0.05% | 70 | 75 | 75.0 | atomspace |

### Phase 3: Specialized AI (Scores 60-75)

| Component | Size | Size% | Complexity | Priority | Final Score | Dependencies |
|-----------|------|--------|------------|----------|-------------|--------------|
| **moses** | 7.9M | 0.34% | 85 | 70 | 69.2 | cogutil |
| **relex** | 1.3M | 0.06% | 75 | 65 | 64.5 | atomspace |

### Phase 4: Language Processing (Scores 45-65)

| Component | Size | Size% | Complexity | Priority | Final Score | Dependencies |
|-----------|------|--------|------------|----------|-------------|--------------|
| **link-grammar** | 29M | 1.26% | 90 | 60 | 60.8 | cogutil |
| **language-learning** | 27M | 1.17% | 85 | 55 | 57.9 | link-grammar, atomspace |
| **lg-atomese** | 388K | 0.02% | 75 | 60 | 57.0 | link-grammar, atomspace |
| **spacetime** | 308K | 0.01% | 70 | 60 | 56.0 | atomspace |

### Phase 5: Persistence & Integration (Scores 40-60)

| Component | Size | Size% | Complexity | Priority | Final Score | Dependencies |
|-----------|------|--------|------------|----------|-------------|--------------|
| **atomspace-rocks** | 872K | 0.04% | 60 | 50 | 52.0 | atomspace |
| **atomspace-cog** | 696K | 0.03% | 65 | 50 | 50.0 | atomspace, cogserver |
| **vision** | 320K | 0.01% | 75 | 50 | 47.5 | atomspace |
| **semantic-vision** | 2.8M | 0.12% | 80 | 45 | 46.4 | vision, atomspace |
| **pattern-index** | 384K | 0.02% | 70 | 45 | 44.0 | atomspace |

### Phase 6: Domain-Specific (Scores 30-45)

| Component | Size | Size% | Complexity | Priority | Final Score | Dependencies |
|-----------|------|--------|------------|----------|-------------|--------------|
| **agi-bio** | 13M | 0.56% | 80 | 35 | 41.3 | atomspace, pln |
| **cheminformatics** | 140K | 0.01% | 85 | 40 | 40.0 | atomspace |
| **destin** | 14M | 0.61% | 90 | 30 | 38.2 | None |
| **rocca** | 5.3M | 0.23% | 85 | 30 | 36.9 | moses |

### Phase 7: Applications & Tools (Scores 15-35)

| Component | Size | Size% | Complexity | Priority | Final Score | Dependencies |
|-----------|------|--------|------------|----------|-------------|--------------|
| **TinyCog** | 147M | 6.39% | 70 | 25 | 32.4 | Simplified OpenCog |
| **loving-ai** | 58M | 2.52% | 60 | 20 | 28.8 | atomspace, opencog |
| **blender_api** | 41M | 1.78% | 80 | 15 | 23.9 | External integration |
| **unity3d-opencog-game** | 139M | 6.04% | 75 | 10 | 21.8 | Game integration |
| **atomspace-explorer** | 15M | 0.65% | 50 | 20 | 20.2 | Web UI |

### Non-Conversion Components

These components will not be converted as they are primarily data, documentation, or external tools:

- **test-datasets** (295M) - Test data files
- **opencog_rpi** (297M) - Hardware-specific binaries  
- **benchmark** (76M) - Performance testing tools
- **external-tools** (59M) - Third-party utilities
- **learn** (708M) - Mostly language models and training data

## Implementation Strategy

### Phase 1: Foundation (Weeks 1-4)
**Goal**: Establish core Crystal infrastructure

1. **cogutil Conversion** (Week 1)
   - Logger system → Crystal logging
   - Config management → Crystal config
   - Random number generation → Crystal random
   - Platform utilities → Crystal equivalents

2. **atomspace Core** (Weeks 2-3)  
   - Atom base classes → Crystal structs/classes
   - Truth values → Crystal value types
   - AtomSpace container → Crystal collections
   - Basic persistence → Crystal serialization

3. **Build System** (Week 4)
   - Crystal shards setup
   - Testing framework
   - CI/CD pipeline
   - Documentation generation

### Phase 2: Core Reasoning (Weeks 5-8)
**Goal**: Implement reasoning engines

1. **opencog Libraries** (Weeks 5-6)
   - Core reasoning algorithms
   - Atom manipulation functions  
   - Query processing foundation

2. **PLN & URE** (Weeks 7-8)
   - Probabilistic Logic Networks
   - Unified Rule Engine
   - Rule-based inference

### Phase 3-7: Incremental Development (Weeks 9-52)
**Goal**: Progressive feature completion

- Follow ranking order within each phase
- Implement components in dependency order
- Maintain compatibility layers during transition
- Add comprehensive testing for each component

## Crystal Language Advantages

### Performance Benefits
- **Zero-cost abstractions**: Compile-time optimizations
- **Memory safety**: No segfaults or memory leaks
- **Concurrency**: Built-in fiber-based concurrency
- **Speed**: Near C++ performance with higher-level syntax

### Development Benefits  
- **Type safety**: Compile-time type checking
- **Null safety**: No null pointer exceptions
- **Metaprogramming**: Powerful macro system
- **Syntax**: Ruby-like readability with C-like performance

### OpenCog-Specific Benefits
- **Pattern matching**: Native support for complex patterns
- **Immutability**: Default immutable data structures  
- **Memory management**: Automatic garbage collection
- **Interoperability**: C library integration for gradual migration

## Development Guidelines

### Code Organization
```
crystalcog/
├── src/
│   ├── cogutil/          # Core utilities
│   ├── atomspace/        # Knowledge representation  
│   ├── opencog/          # Main reasoning libraries
│   ├── pln/              # Probabilistic Logic Networks
│   └── ...               # Other components
├── spec/                 # Test specifications
├── docs/                 # Documentation
└── shard.yml             # Project configuration
```

### Conversion Principles
1. **Preserve Functionality**: All existing features must work
2. **Improve Safety**: Eliminate memory/type safety issues
3. **Enhance Performance**: Leverage Crystal's speed
4. **Maintain APIs**: Keep interface compatibility where possible
5. **Add Tests**: Comprehensive test coverage for all conversions

### Quality Assurance
- **Unit Tests**: Every converted function/class
- **Integration Tests**: Component interaction validation  
- **Performance Tests**: Benchmarking against C++ versions
- **Memory Tests**: Leak detection and usage optimization
- **Continuous Integration**: Automated testing and deployment

## Success Metrics

### Technical Metrics
- **Test Coverage**: >90% code coverage
- **Performance**: Within 10% of C++ performance  
- **Memory Usage**: Comparable or better than C++
- **Build Time**: Faster compilation than C++

### Project Metrics
- **Component Completion**: Track % of components converted
- **API Compatibility**: Measure breaking changes
- **Bug Reduction**: Compare bug reports before/after
- **Development Velocity**: Time to implement new features

## Timeline Summary

- **Phase 1 (Weeks 1-4)**: Foundation - cogutil, atomspace core, build system
- **Phase 2 (Weeks 5-8)**: Core reasoning - opencog, PLN, URE  
- **Phase 3 (Weeks 9-16)**: Specialized AI - moses, asmoses, miner
- **Phase 4 (Weeks 17-28)**: Language processing - link-grammar, language-learning
- **Phase 5 (Weeks 29-36)**: Persistence & integration - storage, networking
- **Phase 6 (Weeks 37-44)**: Domain-specific - bio, chemistry, vision
- **Phase 7 (Weeks 45-52)**: Applications & tools - TinyCog, UIs, games

**Total Estimated Timeline**: 12 months for complete conversion

## Next Steps

1. **Setup Development Environment**
   - Install Crystal compiler
   - Configure development tools
   - Setup project structure

2. **Begin Phase 1 Implementation**  
   - Start with cogutil Logger conversion
   - Create basic Crystal project structure
   - Implement first example conversions

3. **Establish Testing Framework**
   - Setup Crystal spec framework
   - Create conversion validation tests
   - Implement CI/CD pipeline

---

*This roadmap will be updated as the conversion progresses and new insights are gained.*