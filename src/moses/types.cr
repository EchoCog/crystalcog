# MOSES types and core data structures
# Crystal implementation of MOSES core types

module Moses
  # Basic score type - using Float64 for precision in Crystal
  alias Score = Float64
  
  # Special score values
  VERY_BEST_SCORE = Float64::MAX
  VERY_WORST_SCORE = Float64::MIN
  EPSILON_SCORE = Float64::EPSILON
  
  # Helper method to get score or worst score
  def self.score_or_worst(candidate : Candidate) : Score
    candidate.score.try(&.penalized_score) || VERY_WORST_SCORE
  end
  
  # Helper method to safely compare candidates by score
  def self.compare_candidates(a : Candidate, b : Candidate) : Int32
    score_a = score_or_worst(a)
    score_b = score_or_worst(b)
    score_a <=> score_b
  end
  
  # Complexity type for measuring program complexity
  alias Complexity = Int32
  
  # Composite score that includes fitness, complexity, and penalties
  struct CompositeScore
    include Comparable(CompositeScore)
    
    getter score : Score
    getter complexity : Complexity
    getter complexity_penalty : Score
    getter uniformity_penalty : Score
    getter penalized_score : Score
    
    def initialize(@score : Score, @complexity : Complexity, 
                   @complexity_penalty : Score = 0.0, @uniformity_penalty : Score = 0.0)
      @penalized_score = @score - @complexity_penalty - @uniformity_penalty
    end
    
    # Comparison operator for scoring (higher is better)
    def <=>(other : CompositeScore)
      @penalized_score <=> other.penalized_score
    end
    
    def to_s(io)
      io << "CompositeScore(score=#{score}, complexity=#{complexity}, "
      io << "penalized=#{penalized_score})"
    end
  end
  
  # Problem types that MOSES can solve
  enum ProblemType
    BooleanClassification
    Regression
    Clustering
    PatternMining
    FeatureSelection
  end
  
  # Evolution parameters for MOSES
  struct MosesParams
    property problem_type : ProblemType
    property training_data : Array(Array(Float64))
    property target_data : Array(Float64)?
    property max_evals : Int32
    property max_gens : Int32
    property population_size : Int32
    property deme_size : Int32
    property complexity_penalty : Score
    property uniformity_penalty : Score
    property termination_criteria : TerminationCriteria
    
    def initialize(@problem_type : ProblemType, @training_data : Array(Array(Float64)),
                   @max_evals : Int32 = 10000, @max_gens : Int32 = 100,
                   @population_size : Int32 = 100, @deme_size : Int32 = 20,
                   @complexity_penalty : Score = 0.1, @uniformity_penalty : Score = 0.0,
                   @target_data : Array(Float64)? = nil)
      @termination_criteria = TerminationCriteria.new(max_evals, max_gens)
    end
  end
  
  # Termination criteria for evolutionary search
  struct TerminationCriteria
    property max_evals : Int32
    property max_gens : Int32
    property target_score : Score?
    property stagnation_limit : Int32
    
    def initialize(@max_evals : Int32, @max_gens : Int32, 
                   @target_score : Score? = nil, @stagnation_limit : Int32 = 20)
    end
    
    def should_terminate?(evals : Int32, gens : Int32, best_score : Score, 
                         stagnation_count : Int32) : Bool
      return true if evals >= max_evals
      return true if gens >= max_gens
      return true if target_score && best_score >= target_score.not_nil!
      return true if stagnation_count >= stagnation_limit
      false
    end
  end
  
  # Program candidate representation
  # This is a simplified version - in full MOSES this would be a combo tree
  struct Candidate
    property program : String  # For now, use string representation
    property score : CompositeScore?
    property generation : Int32
    property evaluations : Int32
    
    def initialize(@program : String, @generation : Int32 = 0, @evaluations : Int32 = 0)
      @score = nil
    end
    
    def scored?
      !@score.nil?
    end
    
    def to_s(io)
      io << "Candidate(#{program}"
      if scored?
        io << ", score=#{score}"
      end
      io << ")"
    end
  end
  
  # Results from MOSES evolution
  struct MosesResult
    property candidates : Array(Candidate)
    property evaluations : Int32
    property generations : Int32
    property best_score : CompositeScore?
    
    def initialize(@candidates : Array(Candidate), @evaluations : Int32, @generations : Int32)
      scores = candidates.compact_map(&.score)
      @best_score = scores.max? if !scores.empty?
    end
    
    def best_candidate : Candidate?
      return nil if candidates.empty?
      candidates.max_by? { |c| Moses.score_or_worst(c) }
    end
  end
end