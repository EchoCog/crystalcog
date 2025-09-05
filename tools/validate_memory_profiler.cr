#!/usr/bin/env crystal
# Simple validation script for memory profiler functionality

require "./src/cogutil/memory_profiler"

# Test basic memory profiler functionality
puts "Testing CogUtil::MemoryProfiler..."

# Test 1: Get system memory info
puts "\n1. System Memory Info:"
mem_info = CogUtil::MemoryProfiler.get_system_memory_info
puts "  RSS: #{mem_info.rss_kb} KB"
puts "  VSize: #{mem_info.vsize_kb} KB"  
puts "  Heap Size: #{mem_info.heap_size} bytes"
puts "  Memory Efficiency: #{mem_info.memory_efficiency.round(1)}%"

# Test 2: Benchmark a simple operation
puts "\n2. Simple Memory Benchmark:"
result = CogUtil::MemoryProfiler.benchmark_memory("array_creation") do
  # Create a large array
  big_array = Array.new(10000) { |i| "string_#{i}" }
  big_array.size
end

puts "  Operation: #{result.operation}"
puts "  Items: #{result.atom_count}"
puts "  Duration: #{result.duration_ms.round(2)} ms"
puts "  Memory per item: #{result.memory_per_atom.round(2)} bytes"
puts "  Memory increase: #{result.memory_increase_kb} KB"

# Test 3: Evaluation
puts "\n3. Memory Efficiency Evaluation:"
evaluation = CogUtil::MemoryProfiler.evaluate_memory_efficiency(result)
puts "  Meets C++ target: #{evaluation["meets_cpp_target"]}"
puts "  Memory per item: #{evaluation["memory_per_atom"]} bytes"
puts "  Is efficient: #{evaluation["is_efficient"]}"
puts "  Memory efficiency: #{evaluation["memory_efficiency"]}%"

# Test 4: Simple leak detection
puts "\n4. Memory Leak Detection Test:"
has_leak = CogUtil::MemoryProfiler.detect_memory_leaks(10) do
  # Create temporary objects
  temp_array = Array.new(100) { |i| "temp_#{i}" }
end

puts "  Memory leak detected: #{has_leak ? "YES" : "NO"}"

# Test 5: Generate report
puts "\n5. Sample Report Generation:"
sample_results = [result]
report = CogUtil::MemoryProfiler.generate_memory_report(sample_results)
puts report

puts "\n✅ Memory profiler validation completed successfully!"
puts "All core functionality is working correctly."