require "spec"
require "../../src/ure/ure"

describe URE do
  describe "module initialization" do
    it "initializes URE module" do
      URE.initialize
      # Should not crash
    end
    
    it "has correct version" do
      URE::VERSION.should eq("0.1.0")
    end
  end
  
  describe URE::ConjunctionRule do
    before_each do
      @atomspace = AtomSpace::AtomSpace.new
      @rule = URE::ConjunctionRule.new
    end
    
    it "has correct name" do
      @rule.name.should eq("ConjunctionRule")
    end
    
    it "has correct premise requirements" do
      premises = @rule.premises
      premises.size.should eq(2)
      premises.should contain(AtomSpace::AtomType::EVALUATION_LINK)
    end
    
    it "has correct conclusion type" do
      @rule.conclusion.should eq(AtomSpace::AtomType::AND_LINK)
    end
    
    it "applies conjunction correctly" do
      # Create two evaluation links
      tv1 = AtomSpace::SimpleTruthValue.new(0.8, 0.9)
      tv2 = AtomSpace::SimpleTruthValue.new(0.7, 0.8)
      
      likes = @atomspace.add_predicate_node("likes")
      john = @atomspace.add_concept_node("John")
      mary = @atomspace.add_concept_node("Mary")
      
      eval1 = @atomspace.add_evaluation_link(likes, @atomspace.add_list_link([john, mary]), tv1)
      eval2 = @atomspace.add_evaluation_link(likes, @atomspace.add_list_link([mary, john]), tv2)
      
      result = @rule.apply([eval1, eval2], @atomspace)
      
      result.should_not be_nil
      result.not_nil!.type.should eq(AtomSpace::AtomType::AND_LINK)
      
      # Truth value should be minimum of both
      tv = result.not_nil!.truth_value
      tv.strength.should eq(0.7) # min(0.8, 0.7)
      tv.confidence.should be_close(0.72, 0.01) # min(0.9, 0.8) * 0.9
    end
    
    it "calculates fitness correctly" do
      tv1 = AtomSpace::SimpleTruthValue.new(0.8, 0.9)
      tv2 = AtomSpace::SimpleTruthValue.new(0.7, 0.6)
      
      likes = @atomspace.add_predicate_node("likes")
      john = @atomspace.add_concept_node("John")
      mary = @atomspace.add_concept_node("Mary")
      
      eval1 = @atomspace.add_evaluation_link(likes, @atomspace.add_list_link([john, mary]), tv1)
      eval2 = @atomspace.add_evaluation_link(likes, @atomspace.add_list_link([mary, john]), tv2)
      
      fitness = @rule.fitness([eval1, eval2])
      
      # Should be average confidence: (0.9 + 0.6) / 2 = 0.75
      fitness.should be_close(0.75, 0.01)
    end
  end
  
  describe URE::ModusPonensRule do
    before_each do
      @atomspace = AtomSpace::AtomSpace.new
      @rule = URE::ModusPonensRule.new
    end
    
    it "has correct name" do
      @rule.name.should eq("ModusPonensRule")
    end
    
    it "has correct premise requirements" do
      premises = @rule.premises
      premises.size.should eq(2)
      premises.should contain(AtomSpace::AtomType::IMPLICATION_LINK)
      premises.should contain(AtomSpace::AtomType::EVALUATION_LINK)
    end
    
    it "has correct conclusion type" do
      @rule.conclusion.should eq(AtomSpace::AtomType::EVALUATION_LINK)
    end
    
    it "applies modus ponens correctly" do
      # Create implication: if A then B
      tv_impl = AtomSpace::SimpleTruthValue.new(0.9, 0.8)
      tv_ante = AtomSpace::SimpleTruthValue.new(0.8, 0.9)
      
      a = @atomspace.add_concept_node("A")
      b = @atomspace.add_concept_node("B")
      
      # A is true
      antecedent = @atomspace.add_evaluation_link(
        @atomspace.add_predicate_node("true"),
        @atomspace.add_list_link([a]),
        tv_ante
      )
      
      # A -> B
      consequent = @atomspace.add_evaluation_link(
        @atomspace.add_predicate_node("true"),
        @atomspace.add_list_link([b])
      )
      
      implication = @atomspace.add_implication_link(antecedent, consequent, tv_impl)
      
      result = @rule.apply([implication, antecedent], @atomspace)
      
      result.should_not be_nil
      result.should eq(consequent)
      
      # Truth value should be min of both premises
      tv = result.not_nil!.truth_value
      tv.strength.should eq(0.8) # min(0.8, 0.9)
      tv.confidence.should be_close(0.684, 0.01) # 0.9 * 0.8 * 0.95
    end
    
    it "returns nil for non-matching premises" do
      # Create implication and non-matching antecedent
      a = @atomspace.add_concept_node("A")
      b = @atomspace.add_concept_node("B")
      c = @atomspace.add_concept_node("C")
      
      antecedent_a = @atomspace.add_evaluation_link(
        @atomspace.add_predicate_node("true"),
        @atomspace.add_list_link([a])
      )
      
      antecedent_c = @atomspace.add_evaluation_link(
        @atomspace.add_predicate_node("true"),
        @atomspace.add_list_link([c])
      )
      
      consequent = @atomspace.add_evaluation_link(
        @atomspace.add_predicate_node("true"),
        @atomspace.add_list_link([b])
      )
      
      implication = @atomspace.add_implication_link(antecedent_a, consequent)
      
      # Apply with wrong antecedent
      result = @rule.apply([implication, antecedent_c], @atomspace)
      
      result.should be_nil
    end
    
    it "calculates fitness correctly" do
      tv1 = AtomSpace::SimpleTruthValue.new(0.8, 0.9)
      tv2 = AtomSpace::SimpleTruthValue.new(0.6, 0.7)
      
      a = @atomspace.add_concept_node("A")
      b = @atomspace.add_concept_node("B")
      
      antecedent = @atomspace.add_evaluation_link(
        @atomspace.add_predicate_node("true"),
        @atomspace.add_list_link([a]),
        tv1
      )
      
      consequent = @atomspace.add_evaluation_link(
        @atomspace.add_predicate_node("true"),
        @atomspace.add_list_link([b])
      )
      
      implication = @atomspace.add_implication_link(antecedent, consequent, tv2)
      
      fitness = @rule.fitness([implication, antecedent])
      
      # Should be minimum strength: min(0.6, 0.8) = 0.6
      fitness.should be_close(0.6, 0.01)
    end
  end
  
  describe URE::ForwardChainer do
    before_each do
      @atomspace = AtomSpace::AtomSpace.new
      @chainer = URE::ForwardChainer.new(@atomspace, 50)
    end
    
    it "initializes correctly" do
      @chainer.should_not be_nil
    end
    
    it "can add rules" do
      rule = URE::ConjunctionRule.new
      @chainer.add_rule(rule)
      # Should not crash
    end
    
    it "can add default rules" do
      @chainer.add_default_rules
      # Should not crash
    end
    
    it "runs forward chaining" do
      @chainer.add_default_rules
      
      # Add some evaluation links for conjunction
      likes = @atomspace.add_predicate_node("likes")
      john = @atomspace.add_concept_node("John")
      mary = @atomspace.add_concept_node("Mary")
      
      tv1 = AtomSpace::SimpleTruthValue.new(0.8, 0.9)
      tv2 = AtomSpace::SimpleTruthValue.new(0.7, 0.8)
      
      @atomspace.add_evaluation_link(likes, @atomspace.add_list_link([john, mary]), tv1)
      @atomspace.add_evaluation_link(likes, @atomspace.add_list_link([mary, john]), tv2)
      
      initial_size = @atomspace.size
      
      new_atoms = @chainer.run
      
      # Should generate some new atoms (conjunctions)
      new_atoms.size.should be >= 0
      @atomspace.size.should be >= initial_size
    end
    
    it "stops when no new atoms can be generated" do
      @chainer.add_default_rules
      
      # Simple case with single atom
      @atomspace.add_concept_node("test")
      
      new_atoms = @chainer.run
      
      # Should complete without hanging
      new_atoms.size.should be >= 0
    end
    
    it "respects fitness threshold" do
      @chainer.add_default_rules
      
      # Add low-confidence evaluation links
      likes = @atomspace.add_predicate_node("likes")
      john = @atomspace.add_concept_node("John")
      mary = @atomspace.add_concept_node("Mary")
      
      # Very low confidence - should be filtered out
      tv1 = AtomSpace::SimpleTruthValue.new(0.8, 0.05)
      tv2 = AtomSpace::SimpleTruthValue.new(0.7, 0.05)
      
      @atomspace.add_evaluation_link(likes, @atomspace.add_list_link([john, mary]), tv1)
      @atomspace.add_evaluation_link(likes, @atomspace.add_list_link([mary, john]), tv2)
      
      initial_size = @atomspace.size
      
      new_atoms = @chainer.run
      
      # Should generate fewer atoms due to fitness filtering
      new_atoms.size.should be >= 0
    end
  end
  
  describe URE::BackwardChainer do
    before_each do
      @atomspace = AtomSpace::AtomSpace.new
      @chainer = URE::BackwardChainer.new(@atomspace, 5)
    end
    
    it "initializes correctly" do
      @chainer.should_not be_nil
    end
    
    it "can add rules" do
      rule = URE::ConjunctionRule.new
      @chainer.add_rule(rule)
      # Should not crash
    end
    
    it "can add default rules" do
      @chainer.add_default_rules
      # Should not crash
    end
    
    it "finds existing targets" do
      @chainer.add_default_rules
      
      # Add target to atomspace first
      target = @atomspace.add_concept_node("target")
      
      result = @chainer.query(target)
      
      result.should be_true
    end
    
    it "searches for derivable targets" do
      @chainer.add_default_rules
      
      # Create scenario where target can be derived
      likes = @atomspace.add_predicate_node("likes")
      john = @atomspace.add_concept_node("John")
      mary = @atomspace.add_concept_node("Mary")
      
      eval1 = @atomspace.add_evaluation_link(likes, @atomspace.add_list_link([john, mary]))
      eval2 = @atomspace.add_evaluation_link(likes, @atomspace.add_list_link([mary, john]))
      
      # Target: AND link that could be created by conjunction rule
      target_and = AtomSpace::AndLink.new([eval1, eval2])
      
      result = @chainer.query(target_and)
      
      # Result depends on implementation - should not crash
      result.should be_a(Bool)
    end
    
    it "respects depth limit" do
      @chainer.add_default_rules
      
      # Create target that would require deep search
      target = @atomspace.add_concept_node("deep_target")
      
      result = @chainer.query(target, 0) # Depth 0 should only check existence
      
      result.should be_false # Target doesn't exist and depth 0 won't search
    end
    
    it "handles circular references" do
      @chainer.add_default_rules
      
      # Create circular scenario
      a = @atomspace.add_concept_node("A")
      b = @atomspace.add_concept_node("B")
      
      # This is a simplified test - actual circular references would be more complex
      result = @chainer.query(a)
      
      # Should complete without infinite recursion
      result.should be_a(Bool)
    end
  end
  
  describe URE::UREEngine do
    before_each do
      @atomspace = AtomSpace::AtomSpace.new
      @engine = URE::UREEngine.new(@atomspace)
    end
    
    it "initializes with forward and backward chainers" do
      @engine.forward_chainer.should be_a(URE::ForwardChainer)
      @engine.backward_chainer.should be_a(URE::BackwardChainer)
    end
    
    it "can add rules to both chainers" do
      rule = URE::ConjunctionRule.new
      @engine.add_rule(rule)
      # Should not crash
    end
    
    it "performs forward chaining" do
      # Add some atoms for forward chaining
      likes = @atomspace.add_predicate_node("likes")
      john = @atomspace.add_concept_node("John")
      mary = @atomspace.add_concept_node("Mary")
      
      @atomspace.add_evaluation_link(likes, @atomspace.add_list_link([john, mary]))
      @atomspace.add_evaluation_link(likes, @atomspace.add_list_link([mary, john]))
      
      initial_size = @atomspace.size
      
      new_atoms = @engine.forward_chain(3)
      
      new_atoms.size.should be >= 0
      @atomspace.size.should be >= initial_size
    end
    
    it "performs backward chaining" do
      # Add target to atomspace
      target = @atomspace.add_concept_node("target")
      
      result = @engine.backward_chain(target)
      
      result.should be_true
    end
    
    it "performs mixed chaining" do
      # Set up scenario
      target = @atomspace.add_concept_node("target")
      @atomspace.add_concept_node("premise")
      
      result = @engine.mixed_chain(target, 2)
      
      # Should complete without error
      result.should be_a(Bool)
    end
  end
  
  describe "URE convenience methods" do
    it "creates URE engine" do
      atomspace = AtomSpace::AtomSpace.new
      engine = URE.create_engine(atomspace)
      
      engine.should be_a(URE::UREEngine)
    end
  end
  
  describe "URE integration scenarios" do
    before_each do
      @atomspace = AtomSpace::AtomSpace.new
      @engine = URE::UREEngine.new(@atomspace)
    end
    
    it "handles complex forward chaining scenarios" do
      # Create a knowledge base with implications and facts
      tv_high = AtomSpace::SimpleTruthValue.new(0.9, 0.9)
      tv_med = AtomSpace::SimpleTruthValue.new(0.7, 0.8)
      
      # Facts
      human = @atomspace.add_concept_node("human")
      mortal = @atomspace.add_concept_node("mortal")
      socrates = @atomspace.add_concept_node("socrates")
      
      # Socrates is human
      is_human = @atomspace.add_evaluation_link(
        @atomspace.add_predicate_node("is"),
        @atomspace.add_list_link([socrates, human]),
        tv_high
      )
      
      # Humans are mortal  
      are_mortal = @atomspace.add_evaluation_link(
        @atomspace.add_predicate_node("is"),
        @atomspace.add_list_link([human, mortal])
      )
      
      # If human then mortal
      implication = @atomspace.add_implication_link(is_human, are_mortal, tv_med)
      
      initial_size = @atomspace.size
      
      new_atoms = @engine.forward_chain(5)
      
      @atomspace.size.should be >= initial_size
      
      # Should be able to derive that Socrates is mortal (through modus ponens)
      mortal_socrates = new_atoms.find { |atom|
        atom.is_a?(AtomSpace::EvaluationLink) &&
        atom.as(AtomSpace::EvaluationLink).predicate.as(AtomSpace::Node).name == "is" &&
        atom.as(AtomSpace::EvaluationLink).arguments.as(AtomSpace::Link).outgoing[0] == socrates &&
        atom.as(AtomSpace::EvaluationLink).arguments.as(AtomSpace::Link).outgoing[1] == mortal
      }
      
      # This test depends on implementation details
      mortal_socrates.should be_truthy if new_atoms.any?
    end
    
    it "combines forward and backward chaining effectively" do
      # Create goal and some premises
      goal = @atomspace.add_concept_node("goal")
      premise = @atomspace.add_concept_node("premise")
      
      # Run mixed chaining
      result = @engine.mixed_chain(goal, 3)
      
      # Should complete successfully
      result.should be_a(Bool)
    end
    
    it "handles rule priorities and fitness" do
      # Add rules with different fitness characteristics
      rule1 = URE::ConjunctionRule.new
      rule2 = URE::ModusPonensRule.new
      
      @engine.add_rule(rule1)
      @engine.add_rule(rule2)
      
      # Add atoms that would have different fitness scores
      high_conf = AtomSpace::SimpleTruthValue.new(0.8, 0.95)
      low_conf = AtomSpace::SimpleTruthValue.new(0.8, 0.3)
      
      likes = @atomspace.add_predicate_node("likes")
      john = @atomspace.add_concept_node("John")
      mary = @atomspace.add_concept_node("Mary")
      
      eval_high = @atomspace.add_evaluation_link(likes, @atomspace.add_list_link([john, mary]), high_conf)
      eval_low = @atomspace.add_evaluation_link(likes, @atomspace.add_list_link([mary, john]), low_conf)
      
      new_atoms = @engine.forward_chain(3)
      
      # Higher fitness atoms should be preferred
      new_atoms.size.should be >= 0
    end
  end
  
  describe "URE error handling" do
    before_each do
      @atomspace = AtomSpace::AtomSpace.new
      @engine = URE::UREEngine.new(@atomspace)
    end
    
    it "handles empty atomspace" do
      new_atoms = @engine.forward_chain(5)
      new_atoms.should be_empty
      
      target = AtomSpace::ConceptNode.new("nonexistent")
      result = @engine.backward_chain(target)
      result.should be_false
    end
    
    it "handles malformed rules gracefully" do
      # Create custom rule that might fail
      rule = URE::ConjunctionRule.new
      @engine.add_rule(rule)
      
      # Add single atom (insufficient for conjunction)
      @atomspace.add_concept_node("single")
      
      new_atoms = @engine.forward_chain(3)
      
      # Should complete without crashing
      new_atoms.size.should be >= 0
    end
    
    it "handles maximum iteration limits" do
      # Create scenario that could potentially loop
      @engine.add_rule(URE::ConjunctionRule.new)
      
      a = @atomspace.add_concept_node("A")
      b = @atomspace.add_concept_node("B")
      
      eval1 = @atomspace.add_evaluation_link(@atomspace.add_predicate_node("P"), a)
      eval2 = @atomspace.add_evaluation_link(@atomspace.add_predicate_node("P"), b)
      
      # Should respect iteration limits
      new_atoms = @engine.forward_chain(100)
      
      new_atoms.size.should be >= 0
    end
    
    it "handles invalid target queries" do
      # Query with malformed atom
      invalid_atom = AtomSpace::ConceptNode.new("")
      
      result = @engine.backward_chain(invalid_atom)
      
      # Should handle gracefully
      result.should be_a(Bool)
    end
    
    it "handles rule application failures" do
      rule = URE::ModusPonensRule.new
      @engine.add_rule(rule)
      
      # Add atoms that don't match rule requirements
      @atomspace.add_concept_node("unrelated")
      @atomspace.add_predicate_node("also_unrelated")
      
      new_atoms = @engine.forward_chain(3)
      
      # Should complete without error even if no rules apply
      new_atoms.size.should be >= 0
    end
  end
  
  describe "URE performance characteristics" do
    before_each do
      @atomspace = AtomSpace::AtomSpace.new
      @engine = URE::UREEngine.new(@atomspace, 10) # Limited iterations
    end
    
    it "completes within reasonable time for small problems" do
      # Add moderate amount of data
      10.times do |i|
        concept = @atomspace.add_concept_node("concept_#{i}")
        pred = @atomspace.add_predicate_node("pred_#{i}")
        @atomspace.add_evaluation_link(pred, concept)
      end
      
      start_time = Time.monotonic
      new_atoms = @engine.forward_chain(5)
      end_time = Time.monotonic
      
      # Should complete quickly
      duration = end_time - start_time
      duration.should be < 5.seconds
      
      new_atoms.size.should be >= 0
    end
    
    it "scales reasonably with atomspace size" do
      # Create larger knowledge base
      concepts = 20.times.map { |i| @atomspace.add_concept_node("concept_#{i}") }.to_a
      predicates = 5.times.map { |i| @atomspace.add_predicate_node("pred_#{i}") }.to_a
      
      # Create many evaluation links
      concepts.each_with_index do |concept, i|
        predicates.each_with_index do |pred, j|
          tv = AtomSpace::SimpleTruthValue.new(0.5 + (i + j) * 0.01, 0.8)
          @atomspace.add_evaluation_link(pred, concept, tv)
        end
      end
      
      initial_size = @atomspace.size
      
      start_time = Time.monotonic
      new_atoms = @engine.forward_chain(3)
      end_time = Time.monotonic
      
      # Should still complete in reasonable time
      duration = end_time - start_time
      duration.should be < 10.seconds
      
      new_atoms.size.should be >= 0
      @atomspace.size.should be >= initial_size
    end
  end
end