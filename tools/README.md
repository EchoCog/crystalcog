# CrystalCog Tools

This directory contains utility tools for the CrystalCog project.

## Memory Benchmark Tool

### Overview

The `memory_benchmark.cr` tool provides comprehensive memory usage benchmarking for CrystalCog, comparing Crystal implementation performance against C++ OpenCog targets.

### Usage

```bash
# Run all benchmarks
crystal run tools/memory_benchmark.cr

# Run specific benchmark categories
crystal run tools/memory_benchmark.cr -- --basic      # Basic AtomSpace tests
crystal run tools/memory_benchmark.cr -- --scaling    # Memory scaling tests  
crystal run tools/memory_benchmark.cr -- --reasoning  # PLN/URE memory tests
crystal run tools/memory_benchmark.cr -- --leaks      # Memory leak detection

# Show help
crystal run tools/memory_benchmark.cr -- --help
```

### What It Tests

1. **Basic AtomSpace Operations**
   - Node creation memory efficiency
   - Link creation memory usage
   - Complex structure memory overhead

2. **Memory Scaling**
   - Performance with 1K to 20K atoms
   - Memory efficiency at different scales
   - Scaling behavior validation

3. **Reasoning Memory Usage**
   - PLN reasoning memory efficiency
   - URE forward chaining memory usage
   - Inference memory overhead

4. **Memory Leak Detection**
   - Repeated operation leak testing
   - Complex operation leak detection
   - Memory growth monitoring

### Expected Output

```
Crystal CogUtil Memory Benchmark Suite
Comparing Crystal implementation with C++ OpenCog performance

Running Basic AtomSpace Memory Benchmarks...
==================================================

1. Node Creation Benchmark
   Items: 10000
   Duration: 45.23 ms
   Memory/item: 456.78 bytes
   Memory increase: 4456 KB
   C++ target: ✓ PASS
   Efficiency: 87.3%

...

Crystal vs C++ Memory Usage Comparison Report
======================================================================

Overall Assessment:
  Tests passing C++ targets: 8/8 (100%)
  🎉 EXCELLENT: Crystal memory usage is comparable or better than C++

Recommendations:
  ✓ Memory usage target achieved
  ✓ Ready to update roadmap checkbox
```

### Integration

This tool validates the roadmap success metric "Memory usage comparable to C++" and provides ongoing monitoring capabilities for memory performance regression detection.

See `docs/memory_benchmarking.md` for detailed documentation.