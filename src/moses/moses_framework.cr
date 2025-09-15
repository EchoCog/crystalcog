# MOSES Optimization Framework
# Provides high-level API for MOSES evolutionary optimization
# This module wraps the core Moses implementation with a clean, framework-style API

require "./moses"

# Main MOSES framework module (capitalized to match test expectations)  
module MOSES
  VERSION = Moses::VERSION
  
  # Optimizer class that encapsulates the MOSES optimization process
  class Optimizer
    property params : Moses::MosesParams
    property metapopulation : Moses::MetaPopulation?
    property scorer : Moses::ScoringFunction?
    property atomspace : AtomSpace?
    
    def initialize(@params : Moses::MosesParams, @atomspace : AtomSpace? = nil)
      @metapopulation = nil
      @scorer = nil
    end
    
    # Run optimization and return results
    def optimize : Moses::MosesResult
      # Create scorer based on parameters
      @scorer = create_scorer_for_params(@params)
      
      # Create and initialize metapopulation
      @metapopulation = Moses::MetaPopulation.new(@params, @scorer.not_nil!)
      
      # Run the optimization
      best_candidates = @metapopulation.not_nil!.run(@scorer.not_nil!, @params.max_evals)
      
      # Return results
      Moses::MosesResult.new(
        candidates: best_candidates,
        evaluations: @metapopulation.not_nil!.total_evals,
        generations: @metapopulation.not_nil!.generations
      )
    end
    
    # Get current best candidates
    def best_candidates(count : Int32 = 10) : Array(Moses::Candidate)
      @metapopulation.try(&.best_candidates(count)) || [] of Moses::Candidate
    end
    
    # Get optimization statistics
    def statistics : Hash(String, Float64)
      @metapopulation.try(&.calculate_statistics) || {
        "mean_score" => 0.0, "best_score" => 0.0, "worst_score" => 0.0, "diversity" => 0.0
      }
    end
    
    private def create_scorer_for_params(params : Moses::MosesParams) : Moses::ScoringFunction
      case params.problem_type
      when Moses::ProblemType::BooleanClassification
        target_data = params.target_data || (raise Moses::MosesException.new("Boolean classification requires target data"))
        Moses::BooleanTableScoring.new(params.training_data, target_data)
      when Moses::ProblemType::Regression
        target_data = params.target_data || (raise Moses::MosesException.new("Regression requires target data"))
        Moses::RegressionScoring.new(params.training_data, target_data)
      when Moses::ProblemType::Clustering
        Moses::ClusteringScoring.new(params.training_data)
      else
        raise Moses::MosesException.new("Unsupported problem type: #{params.problem_type}")
      end
    end
  end
  
  # Initialize the MOSES framework
  def self.initialize
    Moses.initialize
  end
  
  # Create a MOSES optimizer with default parameters
  def self.create_optimizer(atomspace : AtomSpace? = nil) : Optimizer
    # Create default parameters for demonstration
    default_params = Moses::MosesParams.new(
      problem_type: Moses::ProblemType::BooleanClassification,
      training_data: [[0.0, 0.0], [0.0, 1.0], [1.0, 0.0], [1.0, 1.0]],
      target_data: [0.0, 1.0, 1.0, 0.0],  # XOR function
      max_evals: 100,
      max_gens: 10,
      population_size: 20,
      deme_size: 8
    )
    
    Optimizer.new(default_params, atomspace)
  end
  
  # Create an optimizer with custom parameters
  def self.create_optimizer(params : Moses::MosesParams, atomspace : AtomSpace? = nil) : Optimizer
    Optimizer.new(params, atomspace)
  end
  
  # High-level optimization method
  def self.optimize(params : Moses::MosesParams) : Moses::MosesResult
    optimizer = create_optimizer(params)
    optimizer.optimize
  end
  
  # Create a metapopulation for evolutionary search
  def self.create_metapopulation(params : Moses::MosesParams) : Moses::MetaPopulation
    Moses::MetaPopulation.new(params)
  end
  
  # Create a scoring function for the given problem type and data
  def self.create_scorer(problem_type : Moses::ProblemType, 
                        training_data : Array(Array(Float64)),
                        target_data : Array(Float64)? = nil) : Moses::ScoringFunction
    case problem_type
    when Moses::ProblemType::BooleanClassification
      target = target_data || (raise Moses::MosesException.new("Boolean classification requires target data"))
      Moses::BooleanTableScoring.new(training_data, target)
    when Moses::ProblemType::Regression
      target = target_data || (raise Moses::MosesException.new("Regression requires target data"))
      Moses::RegressionScoring.new(training_data, target)
    when Moses::ProblemType::Clustering
      Moses::ClusteringScoring.new(training_data)
    else
      raise Moses::MosesException.new("Unsupported problem type: #{problem_type}")
    end
  end
  
  # Run a complete MOSES optimization with the given parameters
  def self.run(params : Moses::MosesParams) : Moses::MosesResult
    Moses.run_moses(params)
  end
  
  # Create optimization parameters for boolean classification
  def self.boolean_params(training_data : Array(Array(Float64)), 
                         target_data : Array(Float64),
                         max_evals : Int32 = 500) : Moses::MosesParams
    Moses::MosesParams.new(
      problem_type: Moses::ProblemType::BooleanClassification,
      training_data: training_data,
      target_data: target_data,
      max_evals: max_evals,
      max_gens: 20,
      population_size: 30,
      deme_size: 10
    )
  end
  
  # Create optimization parameters for regression
  def self.regression_params(training_data : Array(Array(Float64)),
                            target_data : Array(Float64),
                            max_evals : Int32 = 300) : Moses::MosesParams
    Moses::MosesParams.new(
      problem_type: Moses::ProblemType::Regression,
      training_data: training_data,
      target_data: target_data,
      max_evals: max_evals,
      max_gens: 15,
      population_size: 20,
      deme_size: 8
    )
  end
  
  # Framework information
  def self.info : Hash(String, String)
    {
      "version" => VERSION,
      "description" => "MOSES Meta-Optimizing Semantic Evolutionary Search Framework",
      "language" => "Crystal",
      "algorithms" => "Evolutionary Programming, Genetic Algorithms, Hill Climbing, Simulated Annealing"
    }
  end
end