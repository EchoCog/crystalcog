# Crystal implementation of Agent-Zero Cognitive Kernel
# Provides hypergraph state management and tensor field encoding
#
# This module implements the cognitive kernel functionality described in
# the Agent-Zero Genesis roadmap, enabling hypergraph state persistence.

require "./atomspace"
require "./storage"
require "../cogutil/cogutil"

module AtomSpace
  # Cognitive kernel for Agent-Zero Genesis system
  class CognitiveKernel
    property atomspace : AtomSpace
    property tensor_shape : Array(Int32)
    property attention_weight : Float64
    property meta_level : Int32
    property cognitive_operation : String?
    
    def initialize(@tensor_shape : Array(Int32), @attention_weight : Float64 = 0.5, 
                   @meta_level : Int32 = 0, @cognitive_operation : String? = nil)
      @atomspace = AtomSpace.new
      
      # Apply attention to atomspace
      CogUtil::Logger.info("CognitiveKernel created: shape=#{@tensor_shape}, attention=#{@attention_weight}")
    end
    
    def initialize(@atomspace : AtomSpace, @tensor_shape : Array(Int32), 
                   @attention_weight : Float64, @meta_level : Int32 = 0, 
                   @cognitive_operation : String? = nil)
      CogUtil::Logger.info("CognitiveKernel created from existing AtomSpace: shape=#{@tensor_shape}")
    end
    
    # Generate tensor field encoding based on mathematical sequences
    def tensor_field_encoding(encoding_type : String = "prime", include_attention : Bool = true,
                             include_meta_level : Bool = false, normalization : String = "none") : Array(Float64)
      
      # Generate base sequence based on encoding type
      base_sequence = case encoding_type.downcase
                     when "prime"
                       generate_primes(tensor_shape.size)
                     when "fibonacci" 
                       generate_fibonacci(tensor_shape.size)
                     when "harmonic"
                       generate_harmonic(tensor_shape.size)
                     when "factorial"
                       generate_factorial(tensor_shape.size)
                     when "power_of_two"
                       generate_powers_of_two(tensor_shape.size)
                     else
                       generate_primes(tensor_shape.size)
                     end
      
      # Apply tensor field encoding (multiply shape by sequence)
      base_encoding = tensor_shape.zip(base_sequence).map { |shape, seq| shape.to_f64 * seq.to_f64 }
      
      # Apply attention weighting if requested
      attention_weighted = if include_attention
                          base_encoding.map { |x| x * attention_weight }
                        else
                          base_encoding
                        end
      
      # Include meta-level information if requested
      meta_enhanced = if include_meta_level
                       attention_weighted + [meta_level.to_f64]
                     else
                       attention_weighted
                     end
      
      # Apply normalization
      normalized = case normalization.downcase
                  when "unit"
                    normalize_to_unit_length(meta_enhanced)
                  when "standard"
                    standardize_encoding(meta_enhanced)
                  else
                    meta_enhanced
                  end
      
      CogUtil::Logger.debug("Generated tensor field encoding: type=#{encoding_type}, size=#{normalized.size}")
      normalized
    end
    
    # Get hypergraph state representation
    def hypergraph_state : HypergraphState
      @atomspace.extract_hypergraph_state(@tensor_shape, @attention_weight, @meta_level, @cognitive_operation)
    end
    
    # Store hypergraph state to storage
    def store_hypergraph_state(storage : HypergraphStateStorageNode) : Bool
      state = hypergraph_state
      storage.store_hypergraph_state(state)
    end
    
    # Load hypergraph state from storage 
    def load_hypergraph_state(storage : HypergraphStateStorageNode) : Bool
      loaded_state = storage.load_hypergraph_state(@atomspace)
      return false unless loaded_state
      
      @tensor_shape = loaded_state.tensor_shape
      @attention_weight = loaded_state.attention
      @meta_level = loaded_state.meta_level
      @cognitive_operation = loaded_state.cognitive_operation
      
      CogUtil::Logger.info("Loaded hypergraph state: shape=#{@tensor_shape}, attention=#{@attention_weight}")
      true
    end
    
    # Create hypergraph-aware tensor encoding
    def hypergraph_tensor_encoding : Array(Float64)
      # Get AtomSpace metrics
      node_count = @atomspace.node_count.to_f64
      link_count = @atomspace.link_count.to_f64
      connectivity = node_count > 0 ? (link_count / node_count) : 0.0
      
      # Base encoding
      base_encoding = tensor_field_encoding("prime", include_attention: false, include_meta_level: false)
      
      # Hypergraph factors
      hypergraph_factors = [connectivity, @attention_weight, @tensor_shape.size.to_f64]
      
      # Combined encoding
      base_encoding + hypergraph_factors
    end
    
    # Cognitive operation-specific encoding
    def cognitive_tensor_field_encoding(operation : String) : Array(Float64)
      base_encoding = tensor_field_encoding
      
      operation_weights = case operation.downcase
                         when "reasoning"
                           [1.5, 1.2, 1.0]
                         when "learning"
                           [1.0, 1.8, 1.3]
                         when "attention"
                           [2.0, 1.0, 1.1]
                         when "memory"
                           [1.1, 1.0, 1.9]
                         when "adaptation"
                           [1.3, 1.6, 1.4]
                         else
                           [1.0, 1.0, 1.0]
                         end
      
      # Apply operation weights cyclically
      weighted_encoding = base_encoding.map_with_index do |val, idx|
        weight_idx = idx % operation_weights.size
        val * operation_weights[weight_idx]
      end
      
      @cognitive_operation = operation
      weighted_encoding
    end
    
    # Mathematical sequence generators
    private def generate_primes(n : Int32) : Array(Float64)
      return [] of Float64 if n <= 0
      
      primes = [] of Float64
      num = 2
      
      while primes.size < n
        if is_prime?(num)
          primes << num.to_f64
        end
        num += 1
      end
      
      primes
    end
    
    private def is_prime?(num : Int32) : Bool
      return false if num <= 1
      return true if num == 2
      return false if num.even?
      
      (3..Math.sqrt(num).to_i).step(2) do |i|
        return false if num % i == 0
      end
      true
    end
    
    private def generate_fibonacci(n : Int32) : Array(Float64)
      return [] of Float64 if n <= 0
      return [1.0] if n == 1
      
      fib = [1.0, 1.0]
      while fib.size < n
        fib << (fib[-1] + fib[-2])
      end
      
      fib[0...n]
    end
    
    private def generate_harmonic(n : Int32) : Array(Float64)
      return [] of Float64 if n <= 0
      (1..n).map { |k| 1.0 / k.to_f64 }
    end
    
    private def generate_factorial(n : Int32) : Array(Float64)
      return [] of Float64 if n <= 0
      
      factorials = [1.0]
      (1...n).each do |i|
        factorials << factorials[-1] * (i + 1).to_f64
      end
      
      factorials
    end
    
    private def generate_powers_of_two(n : Int32) : Array(Float64)
      return [] of Float64 if n <= 0
      (0...n).map { |k| (2 ** k).to_f64 }
    end
    
    # Normalization methods
    private def normalize_to_unit_length(encoding : Array(Float64)) : Array(Float64)
      magnitude = Math.sqrt(encoding.sum { |x| x * x })
      return encoding if magnitude == 0.0
      
      factor = 1.0 / magnitude
      encoding.map { |x| x * factor }
    end
    
    private def standardize_encoding(encoding : Array(Float64)) : Array(Float64)
      return encoding if encoding.empty?
      
      mean = encoding.sum / encoding.size
      centered = encoding.map { |x| x - mean }
      variance = centered.sum { |x| x * x } / encoding.size
      std_dev = Math.sqrt(variance)
      
      return centered if std_dev == 0.0
      
      factor = 1.0 / std_dev
      centered.map { |x| x * factor }
    end
    
    # Convenience methods for AtomSpace operations
    def add_concept_node(name : String, tv : TruthValue = TruthValue::DEFAULT_TV) : Atom
      @atomspace.add_concept_node(name, tv)
    end
    
    def add_predicate_node(name : String, tv : TruthValue = TruthValue::DEFAULT_TV) : Atom
      @atomspace.add_predicate_node(name, tv)
    end
    
    def add_inheritance_link(child : Atom, parent : Atom, tv : TruthValue = TruthValue::DEFAULT_TV) : Atom
      @atomspace.add_inheritance_link(child, parent, tv)
    end
    
    def add_evaluation_link(predicate : Atom, arguments : Atom, tv : TruthValue = TruthValue::DEFAULT_TV) : Atom
      @atomspace.add_evaluation_link(predicate, arguments, tv)
    end
    
    def to_s(io : IO) : Nil
      io << "CognitiveKernel(shape=#{@tensor_shape}, attention=#{@attention_weight}, " \
            "meta_level=#{@meta_level}, atomspace_size=#{@atomspace.size})"
    end
  end
  
  # Manager for multiple cognitive kernels
  class CognitiveKernelManager
    @kernels : Array(CognitiveKernel)
    
    def initialize
      @kernels = [] of CognitiveKernel
    end
    
    def create_kernel(tensor_shape : Array(Int32), attention_weight : Float64 = 0.5) : CognitiveKernel
      kernel = CognitiveKernel.new(tensor_shape, attention_weight)
      @kernels << kernel
      kernel
    end
    
    def adaptive_attention_allocation(goals : Array(String)) : Array(NamedTuple(kernel: CognitiveKernel, attention_score: Float64, activation_priority: Float64, goal: String))
      allocations = [] of NamedTuple(kernel: CognitiveKernel, attention_score: Float64, activation_priority: Float64, goal: String)
      
      @kernels.each_with_index do |kernel, i|
        goal = i < goals.size ? goals[i] : "default"
        score = calculate_attention_score(goal)
        priority = calculate_priority(score)
        
        allocations << {
          kernel: kernel,
          attention_score: score,
          activation_priority: priority,
          goal: goal
        }
      end
      
      allocations
    end
    
    private def calculate_attention_score(goal : String) : Float64
      case goal.downcase
      when "reasoning"
        0.9
      when "learning"
        0.7
      when "attention"
        0.8
      when "memory"
        0.6
      when "adaptation"  
        0.75
      else
        0.5
      end
    end
    
    private def calculate_priority(score : Float64) : Float64
      # Simple priority calculation based on attention score
      score * 0.8 + 0.2 # Ensure minimum priority
    end
    
    def kernels : Array(CognitiveKernel)
      @kernels
    end
    
    def size : Int32
      @kernels.size
    end
  end
end