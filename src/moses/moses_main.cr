# MOSES main entry point
# Command-line interface for running MOSES evolutionary search

require "./moses"
require "./moses_framework"

module Moses
  # Command-line interface for MOSES
  def self.main(args = ARGV)
    puts "MOSES #{VERSION} - Meta-Optimizing Semantic Evolutionary Search"
    puts "Crystal implementation of evolutionary program learning"
    
    case args.first?
    when "demo"
      run_demo
    when "boolean"
      run_boolean_demo
    when "regression"  
      run_regression_demo
    when "test"
      run_test_suite
    else
      puts "Usage: moses [demo|boolean|regression|test]"
      puts "  demo       - Run a comprehensive MOSES demonstration"
      puts "  boolean    - Run boolean classification example"
      puts "  regression - Run regression example"
      puts "  test       - Run MOSES test suite"
    end
  end
  
  # Run a comprehensive MOSES demonstration
  def self.run_demo
    puts "\n=== MOSES Demonstration ==="
    puts "Running boolean classification and regression examples\n"
    
    run_boolean_demo
    puts "\n" + "="*50 + "\n"
    run_regression_demo
    
    puts "\n=== MOSES Demonstration Complete ==="
  end
  
  # Run boolean classification example
  def self.run_boolean_demo
    puts "=== Boolean Classification Example ==="
    
    # Create simple XOR training data
    training_data = [
      [0.0, 0.0],
      [0.0, 1.0], 
      [1.0, 0.0],
      [1.0, 1.0]
    ]
    
    target_data = [0.0, 1.0, 1.0, 0.0]  # XOR outputs
    
    # Create MOSES parameters
    params = MosesParams.new(
      problem_type: ProblemType::BooleanClassification,
      training_data: training_data,
      target_data: target_data,
      max_evals: 500,
      max_gens: 20,
      population_size: 30,
      deme_size: 10
    )
    
    puts "Training data: XOR function"
    training_data.each_with_index do |input, i|
      puts "  #{input} -> #{target_data[i]}"
    end
    
    puts "\nRunning MOSES evolutionary search..."
    result = Moses.run_moses(params)
    
    puts "\nResults:"
    puts "  Evaluations: #{result.evaluations}"
    puts "  Generations: #{result.generations}"
    puts "  Best score: #{result.best_score}"
    
    puts "\nTop candidates:"
    result.candidates[0...5].each_with_index do |candidate, i|
      puts "  #{i + 1}. #{candidate.program} (score: #{candidate.score})"
    end
    
    # Test best candidate
    if best = result.best_candidate
      puts "\nTesting best candidate: #{best.program}"
      # Note: In a full implementation, we would actually execute the program
      puts "  (Program execution not implemented in this demo version)"
    end
  end
  
  # Run regression example
  def self.run_regression_demo
    puts "=== Regression Example ==="
    
    # Create simple linear function training data: y = 2*x + 1
    training_data = (0..10).map { |x| [x.to_f] }.to_a
    target_data = training_data.map { |input| 2.0 * input[0] + 1.0 }
    
    # Create MOSES parameters
    params = MosesParams.new(
      problem_type: ProblemType::Regression,
      training_data: training_data,
      target_data: target_data,
      max_evals: 300,
      max_gens: 15,
      population_size: 20,
      deme_size: 8
    )
    
    puts "Training data: y = 2*x + 1"
    training_data[0...5].each_with_index do |input, i|
      puts "  x=#{input[0]} -> y=#{target_data[i]}"
    end
    puts "  ..."
    
    puts "\nRunning MOSES evolutionary search..."
    result = Moses.run_moses(params)
    
    puts "\nResults:"
    puts "  Evaluations: #{result.evaluations}"
    puts "  Generations: #{result.generations}"
    puts "  Best score: #{result.best_score}"
    
    puts "\nTop candidates:"
    result.candidates[0...5].each_with_index do |candidate, i|
      puts "  #{i + 1}. #{candidate.program} (score: #{candidate.score})"
    end
  end
  
  # Run MOSES test suite
  def self.run_test_suite
    puts "=== MOSES Test Suite ==="
    
    test_count = 0
    passed_count = 0
    
    # Test 1: Basic types
    print "Testing basic types... "
    begin
      score = CompositeScore.new(-0.5, 10, 0.1, 0.0)
      if score.penalized_score == -0.6
        puts "PASSED"
        passed_count += 1
      else
        puts "FAILED - expected -0.6, got #{score.penalized_score}"
      end
    rescue ex
      puts "FAILED - #{ex}"
    end
    test_count += 1
    
    # Test 2: Program generation
    print "Testing program generation... "
    begin
      generator = Representation::ProgramGenerator.new(ProblemType::BooleanClassification, 2)
      program = generator.generate_random
      if program.is_a?(String) && !program.empty?
        puts "PASSED"
        passed_count += 1
      else
        puts "FAILED - invalid program generated"
      end
    rescue ex
      puts "FAILED - #{ex}"
    end
    test_count += 1
    
    # Test 3: Scoring function
    print "Testing scoring function... "
    begin
      training_data = [[0.0, 0.0], [1.0, 1.0]]
      target_data = [0.0, 1.0]
      scoring = BooleanTableScoring.new(training_data, target_data)
      candidate = Candidate.new("$0 and $1")
      score = scoring.score(candidate)
      if score.is_a?(CompositeScore)
        puts "PASSED"
        passed_count += 1
      else
        puts "FAILED - invalid score type"
      end
    rescue ex
      puts "FAILED - #{ex}"
    end
    test_count += 1
    
    # Test 4: Metapopulation creation
    print "Testing metapopulation... "
    begin
      params = MosesParams.new(
        ProblemType::BooleanClassification,
        [[0.0, 0.0], [1.0, 1.0]],
        max_evals: 10,
        population_size: 5,
        target_data: [0.0, 1.0]
      )
      metapop = MetaPopulation.new(params)
      if metapop.size > 0
        puts "PASSED"
        passed_count += 1
      else
        puts "FAILED - metapopulation not created"
      end
    rescue ex
      puts "FAILED - #{ex}"
    end
    test_count += 1
    
    puts "\nTest Results: #{passed_count}/#{test_count} tests passed"
    
    if passed_count == test_count
      puts "All tests PASSED! ✓"
    else
      puts "Some tests FAILED! ✗"
    end
  end
end

# Run if this file is executed directly
if PROGRAM_NAME.includes?("moses")
  Moses.main(ARGV)
end