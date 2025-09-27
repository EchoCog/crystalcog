# Performance regression testing framework for Agent-Zero Genesis
# Ensures optimizations don't regress and performance targets are maintained

require "spec"
require "../src/cogutil/cogutil"
require "../src/cogutil/performance_optimization"
require "../src/atomspace/atomspace_main"
require "../src/atomspace/cognitive_kernel"

CogUtil.initialize
AtomSpace.initialize

describe "Agent-Zero Performance Regression Tests" do
  describe "Memory Pool Performance" do
    it "should maintain allocation speed under load" do
      pool = CogUtil::AtomMemoryPool.new
      
      # Baseline: allocate and deallocate 1000 blocks
      start_time = Time.monotonic
      
      ptrs = [] of Pointer(UInt8)?
      1000.times do
        ptrs << pool.allocate
      end
      
      ptrs.each do |ptr|
        pool.deallocate(ptr) if ptr
      end
      
      duration = (Time.monotonic - start_time).total_milliseconds
      
      # Should complete within 100ms (performance target)
      duration.should be < 100.0
      
      # Pool should maintain good hit rate
      stats = pool.stats
      stats.hit_rate.should be > 80.0  # At least 80% hit rate
      
      puts "Memory Pool Performance: #{duration.round(2)}ms for 1000 allocations/deallocations"
      puts "Hit rate: #{stats.hit_rate.round(1)}%"
    end
    
    it "should handle pool exhaustion gracefully" do
      pool = CogUtil::AtomMemoryPool.new
      ptrs = [] of Pointer(UInt8)?
      
      # Allocate all blocks plus some extra
      (CogUtil::AtomMemoryPool::POOL_SIZE + 100).times do
        ptrs << pool.allocate
      end
      
      # All pointers should be valid
      ptrs.each { |ptr| ptr.should_not be_nil }
      
      # Clean up
      ptrs.each do |ptr|
        pool.deallocate(ptr) if ptr
      end
    end
  end
  
  describe "Cognitive Cache Performance" do
    it "should maintain sub-millisecond lookup times" do
      cache = CogUtil::CognitiveCache(String, Array(Float32)).new(capacity: 10000)
      
      # Populate cache with test data
      1000.times do |i|
        cache["tensor_#{i}"] = Array(Float32).new(100) { |j| (i * j).to_f32 }
      end
      
      # Measure lookup performance
      start_time = Time.monotonic
      
      1000.times do |i|
        result = cache["tensor_#{i % 1000}"]
        result.should_not be_nil
      end
      
      duration = (Time.monotonic - start_time).total_milliseconds
      avg_lookup_time = duration / 1000.0
      
      # Each lookup should be sub-millisecond
      avg_lookup_time.should be < 1.0
      
      # Cache hit rate should be very high for this test
      stats = cache.stats
      stats.hit_rate.should be > 95.0
      
      puts "Cache Performance: #{avg_lookup_time.round(3)}ms average lookup time"
      puts "Hit rate: #{stats.hit_rate.round(1)}%"
    end
    
    it "should handle cache eviction efficiently" do
      cache = CogUtil::CognitiveCache(String, Array(Float32)).new(capacity: 100)
      
      # Add more items than capacity to trigger evictions
      200.times do |i|
        cache["item_#{i}"] = [i.to_f32]
      end
      
      # Cache should not exceed capacity
      cache.size.should be <= 100
      
      stats = cache.stats
      stats.evictions.should be > 0
      
      puts "Cache evictions handled: #{stats.evictions}"
    end
  end
  
  describe "SIMD Optimization Performance" do
    it "should outperform standard operations" do
      size = 10000
      vector_a = Array(Float32).new(size) { |i| i.to_f32 }
      vector_b = Array(Float32).new(size) { |i| (i * 2).to_f32 }
      
      # Measure SIMD dot product
      start_time = Time.monotonic
      simd_result = CogUtil::SIMDOptimizations.dot_product(vector_a, vector_b)
      simd_duration = (Time.monotonic - start_time).total_milliseconds
      
      # Measure standard dot product
      start_time = Time.monotonic
      standard_result = 0.0_f32
      vector_a.each_with_index { |a, i| standard_result += a * vector_b[i] }
      standard_duration = (Time.monotonic - start_time).total_milliseconds
      
      # Results should be approximately equal
      (simd_result - standard_result).abs.should be < 0.1
      
      # SIMD should be faster (at least 50% improvement)
      speedup = standard_duration / simd_duration
      speedup.should be > 1.5
      
      puts "SIMD Speedup: #{speedup.round(2)}x (#{simd_duration.round(3)}ms vs #{standard_duration.round(3)}ms)"
    end
    
    it "should normalize vectors efficiently" do
      vector = Array(Float32).new(1000) { |i| (i + 1).to_f32 }
      
      start_time = Time.monotonic
      normalized = CogUtil::SIMDOptimizations.normalize_l2(vector)
      duration = (Time.monotonic - start_time).total_milliseconds
      
      # Should complete quickly
      duration.should be < 10.0
      
      # Result should be unit length (approximately)
      magnitude = Math.sqrt(normalized.sum { |x| x * x })
      magnitude.should be_close(1.0, 0.001)
      
      puts "Vector normalization: #{duration.round(3)}ms for 1000 elements"
    end
  end
  
  describe "Cognitive Kernel Performance" do
    it "should maintain tensor encoding performance" do
      kernel = AtomSpace::CognitiveKernel.new([128, 128], 0.8)
      
      # Add test data
      50.times do |i|
        kernel.add_concept_node("concept_#{i}")
      end
      
      # Measure tensor encoding performance
      encoding_times = [] of Float64
      
      10.times do
        start_time = Time.monotonic
        encoding = kernel.tensor_field_encoding("prime", true, false, "unit")
        duration = (Time.monotonic - start_time).total_milliseconds
        encoding_times << duration
        
        encoding.should_not be_empty
        encoding.size.should eq(kernel.tensor_shape.size)
      end
      
      avg_time = encoding_times.sum / encoding_times.size
      
      # Should maintain sub-10ms performance for tensor encoding
      avg_time.should be < 10.0
      
      # Check caching effectiveness
      cache_stats = kernel.cache_stats
      cache_stats["cache_hit_rate"].as(Float64).should be > 80.0
      
      puts "Tensor encoding performance: #{avg_time.round(2)}ms average"
      puts "Cache hit rate: #{cache_stats["cache_hit_rate"]}%"
    end
    
    it "should scale linearly with AtomSpace size" do
      kernel = AtomSpace::CognitiveKernel.new([64, 64], 0.8)
      
      performance_data = [] of NamedTuple(size: Int32, duration: Float64)
      
      [10, 50, 100, 200].each do |atom_count|
        # Clear and rebuild AtomSpace
        kernel.atomspace = AtomSpace::AtomSpace.new
        
        atom_count.times do |i|
          kernel.add_concept_node("concept_#{i}")
        end
        
        # Measure hypergraph tensor encoding
        start_time = Time.monotonic
        encoding = kernel.hypergraph_tensor_encoding
        duration = (Time.monotonic - start_time).total_milliseconds
        
        performance_data << {size: atom_count, duration: duration}
        encoding.should_not be_empty
      end
      
      # Performance should scale reasonably (not exponentially)
      performance_data.each_cons(2) do |pair|
        current, next_item = pair
        scale_factor = next_item[:size].to_f / current[:size]
        time_factor = next_item[:duration] / current[:duration]
        
        # Time scaling should be no worse than quadratic
        time_factor.should be < (scale_factor * scale_factor * 1.5)
      end
      
      puts "Scaling performance:"
      performance_data.each do |data|
        puts "  #{data[:size]} atoms: #{data[:duration].round(2)}ms"
      end
    end
  end
  
  describe "Overall System Performance" do
    it "should maintain Agent-Zero performance targets" do
      # Create realistic Agent-Zero scenario
      manager = AtomSpace::CognitiveKernelManager.new
      
      # Create multiple cognitive kernels
      kernels = [] of AtomSpace::CognitiveKernel
      3.times do |i|
        kernel = manager.create_kernel([32, 32], 0.7 + i * 0.1)
        
        # Populate with realistic data
        20.times do |j|
          concept = kernel.add_concept_node("domain_#{i}_concept_#{j}")
          predicate = kernel.add_predicate_node("relation_#{j % 5}")
          
          if j > 0
            parent = kernel.atomspace.get_atoms_by_type(AtomSpace::AtomType::CONCEPT_NODE).sample
            kernel.add_inheritance_link(concept, parent) if parent
          end
        end
        
        kernels << kernel
      end
      
      # Test attention allocation performance
      goals = ["reasoning", "learning", "attention"]
      
      start_time = Time.monotonic
      allocations = manager.adaptive_attention_allocation(goals)
      allocation_duration = (Time.monotonic - start_time).total_milliseconds
      
      allocation_duration.should be < 50.0  # Should be very fast
      allocations.size.should eq(kernels.size)
      
      # Test concurrent tensor operations
      start_time = Time.monotonic
      
      encodings = kernels.map do |kernel|
        kernel.hypergraph_tensor_encoding
      end
      
      concurrent_duration = (Time.monotonic - start_time).total_milliseconds
      
      concurrent_duration.should be < 100.0  # Multiple kernels should still be fast
      encodings.each { |encoding| encoding.should_not be_empty }
      
      puts "Attention allocation: #{allocation_duration.round(2)}ms"
      puts "Concurrent tensor operations: #{concurrent_duration.round(2)}ms"
      
      # Collect overall performance metrics
      total_operations = 0_u64
      total_cache_hits = 0.0
      
      kernels.each do |kernel|
        metrics = kernel.performance_metrics
        metrics.each do |_, metric|
          total_operations += metric.call_count
          total_cache_hits += metric.cache_hit_rate
        end
      end
      
      avg_cache_hit_rate = total_cache_hits / (kernels.size * 3)  # Approximate
      avg_cache_hit_rate.should be > 70.0  # Should maintain good cache performance
      
      puts "Total operations processed: #{total_operations}"
      puts "Average cache hit rate: #{(avg_cache_hit_rate * 100).round(1)}%"
    end
  end
end