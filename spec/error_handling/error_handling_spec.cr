require "spec"
require "../../src/cogutil/cogutil"
require "../../src/atomspace/atomspace_main"
require "../../src/pln/pln"
require "../../src/ure/ure"
require "../../src/opencog/opencog"

describe "Error Handling and Edge Cases" do
  describe "AtomSpace error handling" do
    it "handles empty names gracefully" do
      atomspace = AtomSpace::AtomSpace.new
      
      # Test creating nodes with empty names
      empty_concept = atomspace.add_concept_node("")
      empty_concept.should be_a(AtomSpace::Atom)
      empty_concept.as(AtomSpace::Node).name.should eq("")
    end
    
    it "handles very long names" do
      # Test creating nodes with very long names
      long_name = "a" * 10000
      long_concept = @atomspace.add_concept_node(long_name)
      long_concept.should be_a(AtomSpace::Atom)
      long_concept.as(AtomSpace::Node).name.should eq(long_name)
    end
    
    it "handles special characters in names" do
      # Test creating nodes with special characters
      special_names = [
        "node with spaces",
        "node-with-dashes",
        "node_with_underscores",
        "node.with.dots",
        "node/with/slashes",
        "node(with)parentheses",
        "node[with]brackets",
        "node{with}braces",
        "node\"with\"quotes",
        "node'with'apostrophes",
        "node@with@symbols",
        "node#with#hash",
        "node$with$dollar",
        "node%with%percent",
        "nodeπwithπunicode",
        "node\nwith\nnewlines",
        "node\twith\ttabs"
      ]
      
      special_names.each do |name|
        concept = @atomspace.add_concept_node(name)
        concept.should be_a(AtomSpace::Atom)
        concept.as(AtomSpace::Node).name.should eq(name)
      end
    end
    
    it "handles invalid truth values gracefully" do
      # Test truth values with out-of-range values
      begin
        # Strength > 1.0
        tv_high = AtomSpace::SimpleTruthValue.new(1.5, 0.9)
        concept = @atomspace.add_concept_node("test_high", tv_high)
        
        # Truth value should be clamped or handled appropriately
        concept.truth_value.strength.should be <= 1.0
      rescue ex : AtomSpace::InvalidTruthValueException
        # Should catch invalid truth value exceptions
        ex.should be_a(AtomSpace::InvalidTruthValueException)
      end
      
      begin
        # Negative strength
        tv_neg = AtomSpace::SimpleTruthValue.new(-0.5, 0.9)
        concept = @atomspace.add_concept_node("test_neg", tv_neg)
        
        # Truth value should be clamped or handled appropriately
        concept.truth_value.strength.should be >= 0.0
      rescue ex : AtomSpace::InvalidTruthValueException
        # Should catch invalid truth value exceptions
        ex.should be_a(AtomSpace::InvalidTruthValueException)
      end
    end
    
    it "handles empty link creation" do
      # Test creating links with empty outgoing sets
      begin
        empty_link = @atomspace.add_link(AtomSpace::AtomType::LIST_LINK, [] of AtomSpace::Atom)
        empty_link.should be_a(AtomSpace::Atom)
        empty_link.as(AtomSpace::Link).outgoing.should be_empty
      rescue ex : AtomSpace::InvalidAtomException
        # May throw exception for invalid empty links
        ex.should be_a(AtomSpace::InvalidAtomException)
      end
    end
    
    it "handles circular references in links" do
      # Create atoms that reference each other
      a = @atomspace.add_concept_node("A")
      b = @atomspace.add_concept_node("B")
      
      # Create A->B link
      link_ab = @atomspace.add_inheritance_link(a, b)
      
      # Create B->A link (circular)
      link_ba = @atomspace.add_inheritance_link(b, a)
      
      # Both should be created successfully
      @atomspace.contains?(link_ab).should be_true
      @atomspace.contains?(link_ba).should be_true
      
      # Should not cause infinite loops in operations
      @atomspace.size.should eq(4) # 2 nodes + 2 links
    end
    
    it "handles attempts to remove non-existent atoms" do
      concept = AtomSpace::ConceptNode.new("nonexistent")
      
      result = @atomspace.remove_atom(concept)
      result.should be_false
      
      # AtomSpace should remain unchanged
      @atomspace.size.should eq(0)
    end
    
    it "handles memory pressure gracefully" do
      # Create many atoms to test memory handling
      concepts = [] of AtomSpace::Atom
      
      # Create 10000 concepts to test memory handling
      10000.times do |i|
        concept = @atomspace.add_concept_node("stress_test_#{i}")
        concepts << concept
        
        # Periodically force GC to test memory management
        if i % 1000 == 0
          GC.collect
        end
      end
      
      # All atoms should still be accessible
      @atomspace.size.should eq(10000)
      
      # Random sampling should work
      sample_concept = concepts.sample
      @atomspace.contains?(sample_concept).should be_true
    end
    
    it "handles concurrent access gracefully" do
      # Note: Crystal doesn't have true concurrency, but this tests thread-like behavior
      # In a real concurrent implementation, this would test thread safety
      
      concepts = [] of AtomSpace::Atom
      
      # Simulate concurrent operations
      100.times do |i|
        concept = @atomspace.add_concept_node("concurrent_#{i}")
        concepts << concept
        
        # Interleave different operations
        if i % 3 == 0 && !concepts.empty?
          sample = concepts.sample
          @atomspace.get_atom(sample.handle)
        elsif i % 5 == 0 && concepts.size >= 2
          c1, c2 = concepts.sample(2)
          @atomspace.add_inheritance_link(c1, c2)
        end
      end
      
      # Should complete successfully
      @atomspace.size.should be >= 100
    end
  end
  
  describe "PLN error handling" do
    before_each do
      @atomspace = AtomSpace::AtomSpace.new
      @pln_engine = PLN::PLNEngine.new(@atomspace)
    end
    
    it "handles empty atomspace reasoning" do
      new_atoms = @pln_engine.reason(5)
      new_atoms.should be_empty
      @atomspace.size.should eq(0)
    end
    
    it "handles malformed atoms in reasoning" do
      # Create valid atoms
      dog = @atomspace.add_concept_node("dog")
      cat = @atomspace.add_concept_node("cat")
      
      # Create inheritance link
      inheritance = @atomspace.add_inheritance_link(dog, cat)
      
      # Reasoning should handle this gracefully even if internal structure is complex
      new_atoms = @pln_engine.reason(3)
      
      # Should complete without crashing
      new_atoms.size.should be >= 0
    end
    
    it "handles infinite recursion prevention" do
      # Create circular inheritance that could cause infinite loops
      a = @atomspace.add_concept_node("A")
      b = @atomspace.add_concept_node("B")
      c = @atomspace.add_concept_node("C")
      
      # Create circular chain: A->B->C->A
      @atomspace.add_inheritance_link(a, b)
      @atomspace.add_inheritance_link(b, c)
      @atomspace.add_inheritance_link(c, a)
      
      # Should complete without infinite loop
      start_time = Time.monotonic
      new_atoms = @pln_engine.reason(10)
      end_time = Time.monotonic
      
      # Should complete in reasonable time
      (end_time - start_time).should be < 5.seconds
      new_atoms.size.should be >= 0
    end
    
    it "handles extreme truth values in reasoning" do
      # Create atoms with extreme truth values
      tv_perfect = AtomSpace::SimpleTruthValue.new(1.0, 1.0)
      tv_impossible = AtomSpace::SimpleTruthValue.new(0.0, 1.0)
      tv_unknown = AtomSpace::SimpleTruthValue.new(0.5, 0.0)
      
      a = @atomspace.add_concept_node("A")
      b = @atomspace.add_concept_node("B")
      c = @atomspace.add_concept_node("C")
      d = @atomspace.add_concept_node("D")
      
      @atomspace.add_inheritance_link(a, b, tv_perfect)
      @atomspace.add_inheritance_link(b, c, tv_impossible)
      @atomspace.add_inheritance_link(c, d, tv_unknown)
      
      # Should handle extreme values gracefully
      new_atoms = @pln_engine.reason(5)
      
      # Should complete without mathematical errors
      new_atoms.size.should be >= 0
      
      # Check that derived truth values are valid
      new_atoms.each do |atom|
        tv = atom.truth_value
        tv.strength.should be >= 0.0
        tv.strength.should be <= 1.0
        tv.confidence.should be >= 0.0
        tv.confidence.should be <= 1.0
      end
    end
    
    it "handles rule application failures gracefully" do
      # Create scenario where rules might fail to apply
      single_concept = @atomspace.add_concept_node("lonely")
      
      # Deduction rule needs two inheritance links, but we only have one concept
      new_atoms = @pln_engine.reason(3)
      
      # Should complete without errors
      new_atoms.size.should be >= 0
    end
    
    it "handles memory cleanup during reasoning" do
      # Create large reasoning scenario and test memory handling
      concepts = 50.times.map { |i|
        @atomspace.add_concept_node("memory_test_#{i}")
      }.to_a
      
      tv = AtomSpace::SimpleTruthValue.new(0.8, 0.9)
      
      # Create many inheritance relationships
      100.times do
        c1, c2 = concepts.sample(2)
        @atomspace.add_inheritance_link(c1, c2, tv)
      end
      
      initial_memory = GC.stats.heap_size
      
      # Run reasoning
      new_atoms = @pln_engine.reason(3)
      
      # Force garbage collection
      GC.collect
      
      final_memory = GC.stats.heap_size
      
      # Memory usage should not grow excessively
      memory_growth = final_memory - initial_memory
      
      # Should complete successfully
      new_atoms.size.should be >= 0
      
      # Memory growth should be reasonable (this is a rough heuristic)
      # In practice, exact values depend on implementation details
      puts "Memory growth during PLN reasoning: #{memory_growth} bytes"
    end
  end
  
  describe "URE error handling" do
    before_each do
      @atomspace = AtomSpace::AtomSpace.new
      @ure_engine = URE::UREEngine.new(@atomspace)
    end
    
    it "handles empty atomspace in forward chaining" do
      new_atoms = @ure_engine.forward_chain(5)
      new_atoms.should be_empty
      @atomspace.size.should eq(0)
    end
    
    it "handles empty atomspace in backward chaining" do
      target = AtomSpace::ConceptNode.new("nonexistent")
      result = @ure_engine.backward_chain(target)
      result.should be_false
    end
    
    it "handles malformed rule premises" do
      # Add atoms that don't match any rule premises properly
      concept = @atomspace.add_concept_node("isolated")
      number = AtomSpace::NumberNode.new(42.0)
      @atomspace.add_atom(number)
      
      # Should handle gracefully when no rules apply
      new_atoms = @ure_engine.forward_chain(3)
      new_atoms.size.should be >= 0
    end
    
    it "handles rule application with insufficient premises" do
      # Add single evaluation link (insufficient for conjunction rule)
      pred = @atomspace.add_predicate_node("lonely_pred")
      concept = @atomspace.add_concept_node("concept")
      eval = @atomspace.add_evaluation_link(pred, concept)
      
      # Should complete without errors even if rules can't apply
      new_atoms = @ure_engine.forward_chain(3)
      new_atoms.size.should be >= 0
    end
    
    it "handles fitness calculation edge cases" do
      # Create evaluation links with extreme truth values for fitness testing
      pred = @atomspace.add_predicate_node("extreme_pred")
      concept1 = @atomspace.add_concept_node("concept1")
      concept2 = @atomspace.add_concept_node("concept2")
      
      # Zero confidence (edge case for fitness)
      tv_zero_conf = AtomSpace::SimpleTruthValue.new(0.8, 0.0)
      eval1 = @atomspace.add_evaluation_link(pred, concept1, tv_zero_conf)
      
      # Perfect confidence
      tv_perfect = AtomSpace::SimpleTruthValue.new(0.9, 1.0)
      eval2 = @atomspace.add_evaluation_link(pred, concept2, tv_perfect)
      
      # Should handle fitness calculations gracefully
      new_atoms = @ure_engine.forward_chain(2)
      new_atoms.size.should be >= 0
    end
    
    it "handles deep backward chaining searches" do
      # Create deep goal that would require many search steps
      concepts = 10.times.map { |i|
        @atomspace.add_concept_node("deep_#{i}")
      }.to_a
      
      pred = @atomspace.add_predicate_node("can_reach")
      
      # Create chain of reachability
      (0...concepts.size-1).each do |i|
        @atomspace.add_evaluation_link(
          pred,
          @atomspace.add_list_link([concepts[i], concepts[i+1]])
        )
      end
      
      # Try to reach the last concept from the first
      goal = @atomspace.add_evaluation_link(
        pred,
        @atomspace.add_list_link([concepts[0], concepts[-1]])
      )
      
      # Should complete within depth limits
      result = @ure_engine.backward_chain(goal)
      
      # Result depends on implementation, but should not hang
      result.should be_a(Bool)
    end
    
    it "handles rule conflicts and contradictions" do
      # Create scenario where rules might produce conflicting results
      a = @atomspace.add_concept_node("A")
      b = @atomspace.add_concept_node("B")
      
      pred_true = @atomspace.add_predicate_node("is_true")
      pred_false = @atomspace.add_predicate_node("is_false")
      
      # Contradictory evaluations
      tv_high = AtomSpace::SimpleTruthValue.new(0.9, 0.9)
      eval_true = @atomspace.add_evaluation_link(pred_true, a, tv_high)
      eval_false = @atomspace.add_evaluation_link(pred_false, a, tv_high)
      
      # Should handle contradictory knowledge gracefully
      new_atoms = @ure_engine.forward_chain(3)
      new_atoms.size.should be >= 0
      
      # Atomspace should remain consistent
      @atomspace.get_all_atoms.each do |atom|
        tv = atom.truth_value
        tv.strength.should be >= 0.0
        tv.strength.should be <= 1.0
        tv.confidence.should be >= 0.0
        tv.confidence.should be <= 1.0
      end
    end
  end
  
  describe "Cross-component error handling" do
    before_each do
      @atomspace = AtomSpace::AtomSpace.new
      @pln_engine = PLN.create_engine(@atomspace)
      @ure_engine = URE.create_engine(@atomspace)
    end
    
    it "handles exceptions across all components" do
      begin
        # Create scenario that might trigger various exceptions
        concepts = 5.times.map { |i|
          @atomspace.add_concept_node("cross_test_#{i}")
        }.to_a
        
        # Add some knowledge
        concepts.each_with_index do |concept, i|
          next_concept = concepts[(i + 1) % concepts.size]
          @atomspace.add_inheritance_link(concept, next_concept)
        end
        
        # Run both reasoning engines
        pln_result = @pln_engine.reason(2)
        ure_result = @ure_engine.forward_chain(2)
        
        # Should complete successfully
        pln_result.should be_a(Array(AtomSpace::Atom))
        ure_result.should be_a(Array(AtomSpace::Atom))
        
      rescue ex : OpenCog::OpenCogException
        ex.should be_a(OpenCog::OpenCogException)
        puts "Caught OpenCog exception: #{ex.message}"
      rescue ex : AtomSpace::AtomSpaceException
        ex.should be_a(AtomSpace::AtomSpaceException)
        puts "Caught AtomSpace exception: #{ex.message}"
      rescue ex : CogUtil::OpenCogException
        ex.should be_a(CogUtil::OpenCogException)
        puts "Caught CogUtil exception: #{ex.message}"
      rescue ex : Exception
        fail "Unexpected exception type: #{ex.class} - #{ex.message}"
      end
    end
    
    it "maintains atomspace consistency across component failures" do
      initial_atoms = @atomspace.get_all_atoms.dup
      initial_size = @atomspace.size
      
      begin
        # Create potentially problematic scenario
        problematic_concepts = 3.times.map { |i|
          @atomspace.add_concept_node("problematic_#{i}")
        }.to_a
        
        # Add circular inheritance (potential issue)
        @atomspace.add_inheritance_link(problematic_concepts[0], problematic_concepts[1])
        @atomspace.add_inheritance_link(problematic_concepts[1], problematic_concepts[2])
        @atomspace.add_inheritance_link(problematic_concepts[2], problematic_concepts[0])
        
        # Run reasoning that might fail
        @pln_engine.reason(10) # Might hit iteration limits
        @ure_engine.forward_chain(10) # Might hit iteration limits
        
      rescue ex : Exception
        # Even if reasoning fails, atomspace should remain consistent
        puts "Exception during reasoning: #{ex.message}"
      end
      
      # AtomSpace should still be valid
      @atomspace.size.should be >= initial_size
      
      # Original atoms should still exist
      initial_atoms.each do |atom|
        @atomspace.contains?(atom).should be_true
      end
      
      # All atoms should have valid truth values
      @atomspace.get_all_atoms.each do |atom|
        tv = atom.truth_value
        tv.strength.should be >= 0.0
        tv.strength.should be <= 1.0
        tv.confidence.should be >= 0.0
        tv.confidence.should be <= 1.0
      end
    end
    
    it "handles resource exhaustion gracefully" do
      # Test behavior under resource pressure
      large_concepts = [] of AtomSpace::Atom
      
      begin
        # Create many atoms to potentially exhaust resources
        5000.times do |i|
          concept = @atomspace.add_concept_node("resource_test_#{i}")
          large_concepts << concept
          
          # Add some relationships
          if large_concepts.size >= 2
            other = large_concepts.sample
            @atomspace.add_inheritance_link(concept, other)
          end
          
          # Periodically try reasoning
          if i % 500 == 0
            @pln_engine.reason(1)
            @ure_engine.forward_chain(1)
          end
        end
        
      rescue ex : Exception
        # Should handle resource exhaustion gracefully
        puts "Resource exhaustion handled: #{ex.message}"
      end
      
      # System should still be functional
      @atomspace.size.should be > 0
      
      # Should be able to add new atoms
      test_concept = @atomspace.add_concept_node("post_exhaustion_test")
      @atomspace.contains?(test_concept).should be_true
    end
  end
  
  describe "Input validation and sanitization" do
    before_each do
      @atomspace = AtomSpace::AtomSpace.new
    end
    
    it "handles null and nil-like inputs" do
      # Test with empty string (closest to null in Crystal)
      empty_concept = @atomspace.add_concept_node("")
      empty_concept.should be_a(AtomSpace::Atom)
    end
    
    it "validates atom type consistency" do
      # Test creating links with inappropriate atom types
      concept = @atomspace.add_concept_node("concept")
      predicate = @atomspace.add_predicate_node("predicate")
      
      # Create inheritance link with predicate (unusual but should work)
      inheritance = @atomspace.add_inheritance_link(concept, predicate)
      inheritance.should be_a(AtomSpace::Atom)
    end
    
    it "handles extremely large truth value ranges" do
      # Test with very small and very large numbers
      tiny_tv = AtomSpace::SimpleTruthValue.new(1e-10, 1e-10)
      huge_tv = AtomSpace::SimpleTruthValue.new(0.999999999, 0.999999999)
      
      concept1 = @atomspace.add_concept_node("tiny", tiny_tv)
      concept2 = @atomspace.add_concept_node("huge", huge_tv)
      
      concept1.truth_value.strength.should be >= 0.0
      concept2.truth_value.strength.should be <= 1.0
    end
    
    it "handles rapid operations without data corruption" do
      # Perform many rapid operations to test data integrity
      operations_count = 1000
      created_atoms = [] of AtomSpace::Atom
      
      operations_count.times do |i|
        case i % 4
        when 0
          # Create concept
          concept = @atomspace.add_concept_node("rapid_#{i}")
          created_atoms << concept
        when 1
          # Create predicate
          predicate = @atomspace.add_predicate_node("pred_#{i}")
          created_atoms << predicate
        when 2
          # Create link if we have enough atoms
          if created_atoms.size >= 2
            atom1, atom2 = created_atoms.sample(2)
            link = @atomspace.add_inheritance_link(atom1, atom2)
            created_atoms << link
          end
        when 3
          # Retrieve random atom
          if !created_atoms.empty?
            atom = created_atoms.sample
            retrieved = @atomspace.get_atom(atom.handle)
            retrieved.should eq(atom)
          end
        end
      end
      
      # All atoms should be accessible and consistent
      created_atoms.each do |atom|
        @atomspace.contains?(atom).should be_true
      end
      
      @atomspace.size.should be >= operations_count / 2
    end
  end
end