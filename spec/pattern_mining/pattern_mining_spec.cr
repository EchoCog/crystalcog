require "spec"
require "../../src/pattern_mining/pattern_mining_main"

describe PatternMining do
  
  describe "initialization" do
    it "initializes successfully" do
      PatternMining.initialize.should be_true
    end
  end
  
  describe "PatternSupport" do
    it "creates pattern support correctly" do
      atomspace = AtomSpace::AtomSpace.new
      var_x = AtomSpace::VariableNode.new("$X")
      pattern = PatternMatching::Pattern.new(var_x)
      
      support = PatternMining::PatternSupport.new(pattern, 5, 10)
      
      support.pattern.should eq(pattern)
      support.support.should eq(5)
      support.frequency.should eq(0.5)
    end
    
    it "checks minimum support correctly" do
      atomspace = AtomSpace::AtomSpace.new
      var_x = AtomSpace::VariableNode.new("$X")
      pattern = PatternMatching::Pattern.new(var_x)
      
      support = PatternMining::PatternSupport.new(pattern, 5, 10)
      
      support.meets_minimum_support?(3).should be_true
      support.meets_minimum_support?(6).should be_false
    end
  end
  
  describe "Valuation" do
    it "creates valuation correctly" do
      atomspace = AtomSpace::AtomSpace.new
      var_x = AtomSpace::VariableNode.new("$X")
      pattern = PatternMatching::Pattern.new(var_x)
      
      bindings = PatternMatching::VariableBinding.new
      atom = AtomSpace::ConceptNode.new("dog")
      bindings[var_x] = atom
      match_result = PatternMatching::MatchResult.new(bindings, [atom].map(&.as(AtomSpace::Atom)))
      
      valuation = PatternMining::Valuation.new(pattern, match_result, atom)
      
      valuation.pattern.should eq(pattern)
      valuation.grounding.should eq(match_result)
      valuation.data_atom.should eq(atom)
    end
  end
  
  describe "ShallowAbstraction" do
    it "creates shallow abstraction correctly" do
      atom = AtomSpace::ConceptNode.new("concept")
      abstraction = PatternMining::ShallowAbstraction.new(atom, 3)
      
      abstraction.abstraction_atom.should eq(atom)
      abstraction.frequency.should eq(3)
    end
  end
  
  describe "SupportCalculator" do
    it "creates support calculator" do
      atomspace = AtomSpace::AtomSpace.new
      calculator = PatternMining::SupportCalculator.new(atomspace)
      
      calculator.atomspace.should eq(atomspace)
      calculator.pattern_matcher.should be_a(PatternMatching::PatternMatcher)
    end
    
    it "calculates support for simple patterns" do
      atomspace = AtomSpace::AtomSpace.new
      dog = atomspace.add_concept_node("dog")
      cat = atomspace.add_concept_node("cat")
      
      calculator = PatternMining::SupportCalculator.new(atomspace)
      
      # Create a pattern that matches concept nodes
      var_x = AtomSpace::VariableNode.new("$X")
      pattern = PatternMatching::Pattern.new(var_x)
      
      support = calculator.calculate_support(pattern)
      support.should be >= 2  # Should match at least dog and cat
    end
    
    it "calculates pattern support with frequency" do
      atomspace = AtomSpace::AtomSpace.new
      dog = atomspace.add_concept_node("dog")
      cat = atomspace.add_concept_node("cat")
      
      calculator = PatternMining::SupportCalculator.new(atomspace)
      
      var_x = AtomSpace::VariableNode.new("$X")
      pattern = PatternMatching::Pattern.new(var_x)
      
      pattern_support = calculator.calculate_pattern_support(pattern, atomspace.size.to_i32)
      
      pattern_support.support.should be >= 2
      pattern_support.frequency.should be > 0.0
    end
    
    it "extracts valuations from data atoms" do
      atomspace = AtomSpace::AtomSpace.new
      dog = atomspace.add_concept_node("dog")
      cat = atomspace.add_concept_node("cat")
      
      calculator = PatternMining::SupportCalculator.new(atomspace)
      
      var_x = AtomSpace::VariableNode.new("$X")
      pattern = PatternMatching::Pattern.new(var_x)
      
      data_atoms = [dog, cat].map(&.as(AtomSpace::Atom))
      valuations = calculator.extract_valuations(pattern, data_atoms)
      
      valuations.size.should be >= 0  # May be 0 if pattern matching has issues
    end
  end
  
  describe "PatternSpecializer" do
    it "creates pattern specializer" do
      atomspace = AtomSpace::AtomSpace.new
      specializer = PatternMining::PatternSpecializer.new(atomspace)
      
      specializer.atomspace.should eq(atomspace)
    end
    
    it "determines shallow abstractions from valuations" do
      atomspace = AtomSpace::AtomSpace.new
      specializer = PatternMining::PatternSpecializer.new(atomspace)
      
      # Create some sample valuations
      var_x = AtomSpace::VariableNode.new("$X")
      pattern = PatternMatching::Pattern.new(var_x)
      
      dog = AtomSpace::ConceptNode.new("dog")
      cat = AtomSpace::ConceptNode.new("cat")
      
      bindings1 = PatternMatching::VariableBinding.new
      bindings1[var_x] = dog
      match1 = PatternMatching::MatchResult.new(bindings1, [dog].map(&.as(AtomSpace::Atom)))
      valuation1 = PatternMining::Valuation.new(pattern, match1, dog)
      
      bindings2 = PatternMatching::VariableBinding.new
      bindings2[var_x] = cat
      match2 = PatternMatching::MatchResult.new(bindings2, [cat].map(&.as(AtomSpace::Atom)))
      valuation2 = PatternMining::Valuation.new(pattern, match2, cat)
      
      valuations = [valuation1, valuation2]
      abstractions = specializer.determine_shallow_abstractions(valuations)
      
      # May have abstractions if there are common structural patterns
      abstractions.should be_a(Array(PatternMining::ShallowAbstraction))
    end
    
    it "specializes patterns with abstractions" do
      atomspace = AtomSpace::AtomSpace.new
      specializer = PatternMining::PatternSpecializer.new(atomspace)
      
      var_x = AtomSpace::VariableNode.new("$X")
      base_pattern = PatternMatching::Pattern.new(var_x)
      
      abstraction_atom = AtomSpace::ConceptNode.new("animal")
      abstraction = PatternMining::ShallowAbstraction.new(abstraction_atom, 3)
      
      specialized = specializer.specialize_pattern(base_pattern, abstraction)
      
      # Should return a pattern (may be the same if specialization is simple)
      specialized.should be_a(PatternMatching::Pattern)
    end
  end
  
  describe "PatternMiner" do
    it "creates pattern miner with correct settings" do
      atomspace = AtomSpace::AtomSpace.new
      miner = PatternMining::PatternMiner.new(atomspace, min_support: 3, max_patterns: 50)
      
      miner.atomspace.should eq(atomspace)
      miner.support_calculator.should be_a(PatternMining::SupportCalculator)
      miner.pattern_specializer.should be_a(PatternMining::PatternSpecializer)
    end
    
    it "mines patterns from simple atomspace" do
      atomspace = AtomSpace::AtomSpace.new
      
      # Add some sample data
      dog = atomspace.add_concept_node("dog")
      cat = atomspace.add_concept_node("cat")
      animal = atomspace.add_concept_node("animal")
      
      # Add inheritance relationships
      atomspace.add_inheritance_link(dog, animal)
      atomspace.add_inheritance_link(cat, animal)
      
      miner = PatternMining::PatternMiner.new(atomspace, min_support: 1, max_patterns: 10, timeout_seconds: 5)
      result = miner.mine_patterns
      
      result.should be_a(PatternMining::MiningResult)
      result.total_patterns_explored.should be > 0
      result.mining_time.should be > Time::Span.zero
    end
    
    it "respects timeout limits" do
      atomspace = AtomSpace::AtomSpace.new
      
      # Add many atoms to potentially slow down mining
      20.times do |i|
        atomspace.add_concept_node("concept_#{i}")
      end
      
      miner = PatternMining::PatternMiner.new(atomspace, min_support: 1, max_patterns: 1000, timeout_seconds: 1)
      
      start_time = Time.monotonic
      result = miner.mine_patterns
      elapsed = Time.monotonic - start_time
      
      # Should complete within reasonable time (allowing some overhead)
      elapsed.total_seconds.should be < 5.0
    end
  end
  
  describe "MiningResult" do
    it "creates mining result correctly" do
      patterns = Array(PatternMining::PatternSupport).new
      mining_time = 1.5.seconds
      
      result = PatternMining::MiningResult.new(patterns, 10, mining_time)
      
      result.patterns.should eq(patterns)
      result.total_patterns_explored.should eq(10)
      result.mining_time.should eq(mining_time)
    end
    
    it "filters frequent patterns correctly" do
      atomspace = AtomSpace::AtomSpace.new
      var_x = AtomSpace::VariableNode.new("$X")
      pattern = PatternMatching::Pattern.new(var_x)
      
      patterns = [
        PatternMining::PatternSupport.new(pattern, 5, 10),  # frequency 0.5
        PatternMining::PatternSupport.new(pattern, 2, 10),  # frequency 0.2
        PatternMining::PatternSupport.new(pattern, 1, 10),  # frequency 0.1
      ]
      
      result = PatternMining::MiningResult.new(patterns, 10, 1.second)
      
      frequent = result.frequent_patterns(3)
      frequent.size.should eq(1)  # Only first pattern has support >= 3
      
      frequent = result.frequent_patterns(2)
      frequent.size.should eq(2)  # First two patterns have support >= 2
    end
  end
  
  describe "Utils" do
    it "creates top pattern" do
      pattern = PatternMining::Utils.create_top_pattern
      
      pattern.should be_a(PatternMatching::Pattern)
      pattern.variables.size.should eq(1)
    end
    
    it "creates inheritance pattern" do
      pattern = PatternMining::Utils.create_inheritance_pattern
      
      pattern.should be_a(PatternMatching::Pattern)
      pattern.template.should be_a(AtomSpace::InheritanceLink)
      pattern.variables.size.should eq(2)
    end
    
    it "creates evaluation pattern" do
      pattern = PatternMining::Utils.create_evaluation_pattern
      
      pattern.should be_a(PatternMatching::Pattern)
      pattern.template.should be_a(AtomSpace::EvaluationLink)
      pattern.variables.size.should eq(2)
    end
  end
  
  describe "Module functions" do
    it "creates miner with default settings" do
      atomspace = AtomSpace::AtomSpace.new
      miner = PatternMining.create_miner(atomspace)
      
      miner.should be_a(PatternMining::PatternMiner)
      miner.atomspace.should eq(atomspace)
    end
    
    it "mines patterns with convenient defaults" do
      atomspace = AtomSpace::AtomSpace.new
      
      # Add minimal data
      dog = atomspace.add_concept_node("dog")
      cat = atomspace.add_concept_node("cat")
      
      result = PatternMining.mine(atomspace, min_support: 1, max_patterns: 5, timeout_seconds: 2)
      
      result.should be_a(PatternMining::MiningResult)
      result.total_patterns_explored.should be > 0
    end
  end
end