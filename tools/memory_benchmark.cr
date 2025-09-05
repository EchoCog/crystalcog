#!/usr/bin/env crystal
# Crystal Memory Benchmark Tool
# Provides comprehensive memory benchmarking with C++ OpenCog comparison

require "./src/cogutil/cogutil"
require "./src/atomspace/atomspace"
require "./src/pln/pln"
require "./src/ure/ure"
require "./src/opencog/opencog"

# Memory benchmark runner
class MemoryBenchmarkRunner
  getter results : Array(CogUtil::MemoryProfiler::MemoryBenchmarkResult)
  
  def initialize
    @results = [] of CogUtil::MemoryProfiler::MemoryBenchmarkResult
    CogUtil.initialize
    OpenCog.initialize
  end
  
  def run_basic_atomspace_benchmarks
    puts "Running Basic AtomSpace Memory Benchmarks..."
    puts "=" * 50
    
    atomspace = AtomSpace::AtomSpace.new
    
    # Test 1: Node creation (comparable to C++ addNode benchmark)
    puts "\n1. Node Creation Benchmark"
    result = CogUtil::MemoryProfiler.benchmark_memory("node_creation_10k") do
      10000.times { |i| atomspace.add_concept_node("bench_node_#{i}") }
      10000
    end
    @results << result
    print_result(result)
    
    # Test 2: Link creation (comparable to C++ addLink benchmark)
    puts "\n2. Link Creation Benchmark"
    concepts = (0..1000).map { |i| atomspace.add_concept_node("link_node_#{i}") }
    result = CogUtil::MemoryProfiler.benchmark_memory("link_creation_5k") do
      5000.times do |i|
        source, target = concepts.sample(2)
        atomspace.add_inheritance_link(source, target)
      end
      5000
    end
    @results << result
    print_result(result)
    
    # Test 3: Complex atom structures
    puts "\n3. Complex Structure Benchmark"
    result = CogUtil::MemoryProfiler.benchmark_memory("complex_structures_2k") do
      2000.times do |i|
        pred = atomspace.add_predicate_node("pred_#{i}")
        arg1 = atomspace.add_concept_node("arg1_#{i}")
        arg2 = atomspace.add_concept_node("arg2_#{i}")
        
        list = atomspace.add_list_link([arg1, arg2])
        eval = atomspace.add_evaluation_link(pred, list)
        
        tv = AtomSpace::SimpleTruthValue.new(rand * 0.5 + 0.5, rand * 0.3 + 0.7)
        eval.truth_value = tv
      end
      2000
    end
    @results << result
    print_result(result)
    
    atomspace
  end
  
  def run_scaling_benchmarks
    puts "\n\nRunning Scaling Benchmarks..."
    puts "=" * 50
    
    atomspace = AtomSpace::AtomSpace.new
    
    # Test different scales like C++ benchmark (which tests up to 256K atoms)
    scale_factors = [1000, 5000, 10000, 20000]
    
    scale_factors.each do |scale|
      puts "\n4. Scaling Test - #{scale} atoms"
      result = CogUtil::MemoryProfiler.benchmark_memory("scaling_#{scale}") do
        scale.times { |i| atomspace.add_concept_node("scale_#{scale}_#{i}") }
        scale
      end
      @results << result
      print_result(result)
      
      # Clear for next test
      atomspace.clear
    end
  end
  
  def run_reasoning_benchmarks
    puts "\n\nRunning Reasoning Memory Benchmarks..."
    puts "=" * 50
    
    atomspace = AtomSpace::AtomSpace.new
    
    # PLN reasoning memory test
    puts "\n5. PLN Reasoning Memory"
    pln_engine = PLN::PLNEngine.new(atomspace)
    
    # Setup reasoning knowledge base
    tv = AtomSpace::SimpleTruthValue.new(0.8, 0.9)
    concepts = 20.times.map { |i| atomspace.add_concept_node("pln_#{i}") }.to_a
    
    # Create chains for reasoning
    15.times do |i|
      source, target = concepts.sample(2)
      atomspace.add_inheritance_link(source, target, tv)
    end
    
    initial_size = atomspace.size
    result = CogUtil::MemoryProfiler.benchmark_memory("pln_reasoning") do
      new_atoms = pln_engine.reason(10)
      new_atoms.size
    end
    @results << result
    print_result(result)
    
    # URE reasoning memory test
    puts "\n6. URE Reasoning Memory"
    atomspace.clear
    ure_engine = URE::UREEngine.new(atomspace)
    
    # Setup URE knowledge base
    predicates = 5.times.map { |i| atomspace.add_predicate_node("ure_pred_#{i}") }.to_a
    entities = 30.times.map { |i| atomspace.add_concept_node("ure_entity_#{i}") }.to_a
    
    50.times do
      pred = predicates.sample
      arg1, arg2 = entities.sample(2)
      list = atomspace.add_list_link([arg1, arg2])
      eval = atomspace.add_evaluation_link(pred, list)
      eval.truth_value = AtomSpace::SimpleTruthValue.new(0.7, 0.8)
    end
    
    result = CogUtil::MemoryProfiler.benchmark_memory("ure_reasoning") do
      new_atoms = ure_engine.forward_chain(5)
      new_atoms.size
    end
    @results << result
    print_result(result)
  end
  
  def run_memory_leak_tests
    puts "\n\nRunning Memory Leak Detection..."
    puts "=" * 50
    
    atomspace = AtomSpace::AtomSpace.new
    
    puts "\n7. Memory Leak Detection"
    
    # Test for leaks in repeated operations
    has_leak = CogUtil::MemoryProfiler.detect_memory_leaks(200) do
      20.times do |i|
        node = atomspace.add_concept_node("temp_#{i}")
        tv = AtomSpace::SimpleTruthValue.new(rand, rand)
        node.truth_value = tv
      end
    end
    
    puts "   Memory leak detected: #{has_leak ? "YES (⚠️  ISSUE)" : "NO (✓ GOOD)"}"
    
    # Test for leaks in complex operations
    has_complex_leak = CogUtil::MemoryProfiler.detect_memory_leaks(100) do
      50.times do |i|
        concepts = 5.times.map { |j| atomspace.add_concept_node("complex_#{i}_#{j}") }.to_a
        10.times do |k|
          source, target = concepts.sample(2)
          atomspace.add_inheritance_link(source, target)
        end
      end
    end
    
    puts "   Complex operation leaks: #{has_complex_leak ? "YES (⚠️  ISSUE)" : "NO (✓ GOOD)"}"
  end
  
  def generate_comparison_report
    puts "\n\nCrystal vs C++ Memory Usage Comparison Report"
    puts "=" * 70
    
    report = CogUtil::MemoryProfiler.generate_memory_report(@results)
    puts report
    
    # Additional C++ comparison analysis
    puts "\nC++ OpenCog Benchmark Comparison:"
    puts "-" * 40
    
    # Analyze against typical C++ performance
    cpp_node_memory = 500.0  # Typical C++ node memory usage (bytes)
    cpp_link_memory = 800.0  # Typical C++ link memory usage (bytes)
    
    node_results = @results.select { |r| r.operation.includes?("node") }
    link_results = @results.select { |r| r.operation.includes?("link") }
    
    if !node_results.empty?
      avg_node_memory = node_results.map(&.memory_per_atom).sum / node_results.size
      node_comparison = (avg_node_memory / cpp_node_memory) * 100.0
      
      puts "Node Memory Comparison:"
      puts "  Crystal average: #{avg_node_memory.round(2)} bytes/node"
      puts "  C++ typical: #{cpp_node_memory} bytes/node"
      puts "  Crystal vs C++: #{node_comparison.round(1)}% #{node_comparison < 100 ? "(✓ BETTER)" : node_comparison < 120 ? "(✓ COMPARABLE)" : "(⚠️  HIGHER)"}"
    end
    
    if !link_results.empty?
      avg_link_memory = link_results.map(&.memory_per_atom).sum / link_results.size
      link_comparison = (avg_link_memory / cpp_link_memory) * 100.0
      
      puts "Link Memory Comparison:"
      puts "  Crystal average: #{avg_link_memory.round(2)} bytes/link"
      puts "  C++ typical: #{cpp_link_memory} bytes/link"
      puts "  Crystal vs C++: #{link_comparison.round(1)}% #{link_comparison < 100 ? "(✓ BETTER)" : link_comparison < 120 ? "(✓ COMPARABLE)" : "(⚠️  HIGHER)"}"
    end
    
    # Overall assessment
    total_evaluations = @results.map { |r| CogUtil::MemoryProfiler.evaluate_memory_efficiency(r) }
    passing_count = total_evaluations.count { |e| e["meets_cpp_target"].as(Bool) }
    pass_rate = (passing_count.to_f / total_evaluations.size.to_f) * 100.0
    
    puts "\nOverall Assessment:"
    puts "  Tests passing C++ targets: #{passing_count}/#{total_evaluations.size} (#{pass_rate.round(1)}%)"
    
    if pass_rate >= 90.0
      puts "  🎉 EXCELLENT: Crystal memory usage is comparable or better than C++"
    elsif pass_rate >= 75.0
      puts "  ✅ GOOD: Crystal memory usage is mostly comparable to C++"
    elsif pass_rate >= 50.0
      puts "  ⚠️  FAIR: Crystal memory usage needs some optimization"
    else
      puts "  ❌ POOR: Crystal memory usage significantly higher than C++"
    end
    
    puts "\nRecommendations:"
    if pass_rate >= 90.0
      puts "  ✓ Memory usage target achieved"
      puts "  ✓ Ready to update roadmap checkbox"
    elsif pass_rate >= 75.0
      puts "  → Minor optimizations recommended"
      puts "  → Consider updating roadmap with caveats"
    else
      puts "  → Significant memory optimization needed"
      puts "  → Review memory allocation patterns"
      puts "  → Consider memory pooling for frequent allocations"
    end
  end
  
  private def print_result(result)
    evaluation = CogUtil::MemoryProfiler.evaluate_memory_efficiency(result)
    status = evaluation["meets_cpp_target"] ? "✓ PASS" : "⚠️  REVIEW"
    
    puts "   Items: #{result.atom_count}"
    puts "   Duration: #{result.duration_ms.round(2)} ms"
    puts "   Memory/item: #{result.memory_per_atom.round(2)} bytes"
    puts "   Memory increase: #{result.memory_increase_kb} KB"
    puts "   C++ target: #{status}"
    puts "   Efficiency: #{result.memory_efficiency.round(1)}%"
  end
end

# Main execution
if ARGV.includes?("--help") || ARGV.includes?("-h")
  puts "Crystal Memory Benchmark Tool"
  puts "Usage: crystal run memory_benchmark.cr [options]"
  puts ""
  puts "Options:"
  puts "  --basic      Run only basic AtomSpace benchmarks"
  puts "  --scaling    Run only scaling benchmarks"
  puts "  --reasoning  Run only reasoning benchmarks"
  puts "  --leaks      Run only memory leak detection"
  puts "  --all        Run all benchmarks (default)"
  puts "  --help, -h   Show this help"
  exit 0
end

runner = MemoryBenchmarkRunner.new

puts "Crystal CogUtil Memory Benchmark Suite"
puts "Comparing Crystal implementation with C++ OpenCog performance"
puts "Time: #{Time.local}"
puts ""

if ARGV.includes?("--basic") || ARGV.empty? || ARGV.includes?("--all")
  runner.run_basic_atomspace_benchmarks
end

if ARGV.includes?("--scaling") || ARGV.empty? || ARGV.includes?("--all")
  runner.run_scaling_benchmarks
end

if ARGV.includes?("--reasoning") || ARGV.empty? || ARGV.includes?("--all")
  runner.run_reasoning_benchmarks
end

if ARGV.includes?("--leaks") || ARGV.empty? || ARGV.includes?("--all")
  runner.run_memory_leak_tests
end

runner.generate_comparison_report

puts "\n" + "=" * 70
puts "Memory benchmark completed successfully!"
puts "Crystal memory usage is being compared against C++ OpenCog targets."
puts "=" * 70