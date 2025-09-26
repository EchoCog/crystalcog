require "benchmark"
require "json"
require "../../src/cogutil/cogutil"
require "../../src/atomspace/atomspace_main"
require "../../src/pln/pln"
require "../../src/pattern_matching/pattern_matching"

# Comprehensive performance benchmarks for CrystalCog
class CrystalCogBenchmarks
  @results : Hash(String, Hash(String, Float64))

  def initialize
    @results = Hash(String, Hash(String, Float64)).new
    setup_environment
  end

  def setup_environment
    CogUtil.initialize
    AtomSpace.initialize
    PLN.initialize
  end

  def run_all_benchmarks
    puts "CrystalCog Comprehensive Performance Benchmarks"
    puts "=============================================="
    puts "Date: #{Time.utc}"
    puts "Crystal Version: #{Crystal::VERSION}"
    puts ""

    benchmark_atomspace_operations
    benchmark_pln_reasoning
    benchmark_pattern_matching
    benchmark_memory_usage
    benchmark_concurrent_operations

    generate_report
  end

  def benchmark_atomspace_operations
    puts "📊 AtomSpace Operations Benchmarks"
    puts "-----------------------------------"

    atomspace = AtomSpace::AtomSpace.new
    @results["atomspace"] = Hash(String, Float64).new

    # Node creation benchmark
    result = Benchmark.measure do
      1000.times do |i|
        atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "node_#{i}")
      end
    end
    @results["atomspace"]["node_creation_1k"] = result.total
    puts "Node creation (1k): #{result.total.round(4)}s"

    # Link creation benchmark
    nodes = atomspace.get_atoms_by_type(AtomSpace::AtomType::CONCEPT_NODE)
    result = Benchmark.measure do
      500.times do |i|
        node1 = nodes[i % nodes.size]
        node2 = nodes[(i + 1) % nodes.size]
        atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [node1, node2])
      end
    end
    @results["atomspace"]["link_creation_500"] = result.total
    puts "Link creation (500): #{result.total.round(4)}s"

    # Lookup benchmark
    test_node = nodes.first
    result = Benchmark.measure do
      10000.times do |i|
        atomspace.contains?(test_node)
      end
    end
    @results["atomspace"]["lookup_10k"] = result.total
    puts "Lookups (10k): #{result.total.round(4)}s"

    # Type filtering benchmark
    result = Benchmark.measure do
      100.times do |i|
        atomspace.get_atoms_by_type(AtomSpace::AtomType::CONCEPT_NODE)
      end
    end
    @results["atomspace"]["type_filtering_100"] = result.total
    puts "Type filtering (100): #{result.total.round(4)}s"

    puts "AtomSpace final size: #{atomspace.size} atoms"
    puts ""
  end

  def benchmark_pln_reasoning
    puts "🧠 PLN Reasoning Benchmarks"
    puts "---------------------------"

    atomspace = AtomSpace::AtomSpace.new
    reasoner = PLN::Reasoner.new(atomspace)
    @results["pln"] = Hash(String, Float64).new

    # Create knowledge base
    create_knowledge_base(atomspace)
    
    # Single reasoning step benchmark
    result = Benchmark.measure do
      10.times { reasoner.step_forward }
    end
    @results["pln"]["single_steps_10"] = result.total
    puts "Single reasoning steps (10): #{result.total.round(4)}s"

    # Multi-step reasoning benchmark
    result = Benchmark.measure do
      reasoner.reason(50)
    end
    @results["pln"]["multi_step_50"] = result.total
    puts "Multi-step reasoning (50): #{result.total.round(4)}s"

    # Complex query benchmark
    dog = atomspace.get_atoms_by_name("dog").first
    var = atomspace.add_node(AtomSpace::AtomType::VARIABLE_NODE, "$x")
    query = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, var])
    
    result = Benchmark.measure do
      20.times { reasoner.query(query) }
    end
    @results["pln"]["complex_queries_20"] = result.total
    puts "Complex queries (20): #{result.total.round(4)}s"

    puts "Final AtomSpace size after reasoning: #{atomspace.size} atoms"
    puts ""
  end

  def benchmark_pattern_matching
    puts "🔍 Pattern Matching Benchmarks"
    puts "------------------------------"

    atomspace = AtomSpace::AtomSpace.new
    @results["pattern_matching"] = Hash(String, Float64).new

    # Create test data
    create_pattern_test_data(atomspace)
    
    matcher = PatternMatching::PatternMatcher.new(atomspace)

    # Simple pattern matching
    dog = atomspace.get_atoms_by_name("dog").first
    var = atomspace.add_node(AtomSpace::AtomType::VARIABLE_NODE, "$parent")
    pattern = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, var])
    
    result = Benchmark.measure do
      100.times { matcher.match(pattern) }
    end
    @results["pattern_matching"]["simple_patterns_100"] = result.total
    puts "Simple patterns (100): #{result.total.round(4)}s"

    # Complex pattern matching with multiple variables
    var1 = atomspace.add_node(AtomSpace::AtomType::VARIABLE_NODE, "$child")
    var2 = atomspace.add_node(AtomSpace::AtomType::VARIABLE_NODE, "$parent")
    complex_pattern = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [var1, var2])
    
    result = Benchmark.measure do
      20.times { matcher.match(complex_pattern) }
    end
    @results["pattern_matching"]["complex_patterns_20"] = result.total
    puts "Complex patterns (20): #{result.total.round(4)}s"

    # Query builder performance
    builder = PatternMatching::QueryBuilder.new(atomspace)
    
    result = Benchmark.measure do
      50.times do
        query = builder
          .select(["$x"])
          .where("$x", "isa", "mammal")
          .build
        matcher.execute_query(query)
      end
    end
    @results["pattern_matching"]["query_builder_50"] = result.total
    puts "Query builder patterns (50): #{result.total.round(4)}s"

    puts ""
  end

  def benchmark_memory_usage
    puts "💾 Memory Usage Benchmarks"
    puts "-------------------------"

    @results["memory"] = Hash(String, Float64).new

    # Memory usage during large AtomSpace creation
    initial_memory = get_memory_usage
    
    atomspace = AtomSpace::AtomSpace.new
    
    # Create large number of atoms
    5000.times do |i|
      node = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "large_test_#{i}")
      
      # Add some links
      if i > 0 && i % 100 == 0
        prev_node = atomspace.get_atoms_by_name("large_test_#{i-1}").first
        atomspace.add_link(AtomSpace::AtomType::SIMILARITY_LINK, [node, prev_node])
      end
    end

    final_memory = get_memory_usage
    memory_increase = final_memory - initial_memory
    
    @results["memory"]["atomspace_5k_atoms_mb"] = memory_increase / (1024.0 * 1024.0)
    puts "Memory for 5k atoms: #{(memory_increase / (1024.0 * 1024.0)).round(2)} MB"
    puts "Memory per atom: #{(memory_increase / 5000.0).round(2)} bytes"

    # Memory after garbage collection
    GC.collect
    gc_memory = get_memory_usage
    memory_freed = final_memory - gc_memory
    
    @results["memory"]["gc_freed_mb"] = memory_freed / (1024.0 * 1024.0)
    puts "Memory freed by GC: #{(memory_freed / (1024.0 * 1024.0)).round(2)} MB"

    puts ""
  end

  def benchmark_concurrent_operations
    puts "⚡ Concurrent Operations Benchmarks"
    puts "----------------------------------"

    @results["concurrent"] = Hash(String, Float64).new

    # Concurrent AtomSpace operations
    atomspace = AtomSpace::AtomSpace.new
    
    result = Benchmark.measure do
      channels = [] of Channel(Nil)
      
      4.times do |worker_id|
        channel = Channel(Nil).new
        channels << channel
        
        spawn do
          250.times do |i|
            node_name = "worker_#{worker_id}_node_#{i}"
            atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, node_name)
          end
          channel.send(nil)
        end
      end
      
      # Wait for all workers to complete
      channels.each(&.receive)
    end
    
    @results["concurrent"]["parallel_node_creation"] = result.total
    puts "Parallel node creation (4 workers, 250 each): #{result.total.round(4)}s"
    puts "Final atomspace size: #{atomspace.size} atoms"

    # Concurrent reasoning
    create_knowledge_base(atomspace)
    reasoner = PLN::Reasoner.new(atomspace)
    
    result = Benchmark.measure do
      channels = [] of Channel(Nil)
      
      2.times do |worker_id|
        channel = Channel(Nil).new
        channels << channel
        
        spawn do
          10.times { reasoner.step_forward }
          channel.send(nil)
        end
      end
      
      channels.each(&.receive)
    end
    
    @results["concurrent"]["parallel_reasoning"] = result.total
    puts "Parallel reasoning (2 workers, 10 steps each): #{result.total.round(4)}s"

    puts ""
  end

  private def create_knowledge_base(atomspace)
    # Create a realistic knowledge base for testing
    animals = ["dog", "cat", "bird", "fish", "snake"]
    categories = ["mammal", "vertebrate", "animal", "living_thing"]
    
    # Create nodes
    animal_nodes = animals.map do |animal|
      atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, animal)
    end
    
    category_nodes = categories.map do |category|
      atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, category)
    end
    
    # Create inheritance relationships
    # Animals -> categories
    animal_nodes.each do |animal|
      # Each animal inherits from at least one category
      category = category_nodes.sample
      link = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [animal, category])
      link.set_truth_value(0.8 + rand * 0.2, 0.7 + rand * 0.3)
    end
    
    # Category hierarchy
    (0...category_nodes.size - 1).each do |i|
      link = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, 
                               [category_nodes[i], category_nodes[i + 1]])
      link.set_truth_value(0.9, 0.8)
    end
  end

  private def create_pattern_test_data(atomspace)
    # Create test data for pattern matching
    entities = ["dog", "cat", "bird", "car", "house", "tree"]
    properties = ["mammal", "vehicle", "living", "artificial", "natural"]
    
    entities.each do |entity|
      entity_node = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, entity)
      
      # Add some properties
      properties.sample(2).each do |property|
        property_node = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, property)
        atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [entity_node, property_node])
      end
    end
  end

  private def get_memory_usage : Int64
    # Platform-specific memory usage detection
    {% if flag?(:linux) %}
      if status = File.read("/proc/self/status")
        if match = status.match(/VmRSS:\s*(\d+)\s*kB/)
          return match[1].to_i64 * 1024
        end
      end
    {% end %}
    
    # Fallback to GC stats
    GC.stats.heap_size.to_i64
  rescue
    0_i64
  end

  def generate_report
    puts "📋 Benchmark Summary Report"
    puts "============================"
    
    report = {
      "timestamp" => Time.utc.to_s,
      "crystal_version" => Crystal::VERSION,
      "results" => @results
    }
    
    # Calculate some aggregate metrics
    total_time = @results.values.map(&.values.sum).sum
    puts "Total benchmark time: #{total_time.round(4)}s"
    
    # Find bottlenecks
    all_results = [] of {String, Float64}
    @results.each do |category, tests|
      tests.each do |test, time|
        all_results << {"#{category}.#{test}", time}
      end
    end
    
    slowest = all_results.sort_by(&.[1]).reverse[0..4]
    puts "\nSlowest operations:"
    slowest.each_with_index do |result, i|
      puts "  #{i + 1}. #{result[0]}: #{result[1].round(4)}s"
    end
    
    # Save detailed report
    report_file = "tests/reports/performance/benchmark-#{Time.utc.to_s("%Y%m%d-%H%M%S")}.json"
    File.write(report_file, report.to_pretty_json)
    puts "\nDetailed report saved to: #{report_file}"
    
    puts "\n🎯 Performance Analysis Complete!"
  end
end

# Run benchmarks if executed directly
if PROGRAM_NAME.includes?("comprehensive_benchmarks")
  benchmarks = CrystalCogBenchmarks.new
  benchmarks.run_all_benchmarks
end