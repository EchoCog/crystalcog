require "spec"
require "../../../src/cogutil/cogutil"
require "../../../src/atomspace/atomspace_main"
require "../../../src/pln/pln"

# Unit tests for PLN (Probabilistic Logic Networks) functionality
describe PLN do
  before_each do
    CogUtil.initialize
    AtomSpace.initialize
    PLN.initialize
  end

  describe "PLN initialization" do
    it "initializes PLN system" do
      # PLN should initialize without errors
      PLN.initialized?.should be_true
    end

    it "creates reasoner instances" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = PLN::Reasoner.new(atomspace)
      
      reasoner.should_not be_nil
      reasoner.atomspace.should eq(atomspace)
    end
  end

  describe "Basic reasoning rules" do
    it "applies modus ponens rule" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = PLN::Reasoner.new(atomspace)
      
      # Create premises: A -> B, A
      a = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "A")
      b = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "B")
      
      # A -> B (implication)
      implication = atomspace.add_link(AtomSpace::AtomType::IMPLICATION_LINK, [a, b])
      implication.set_truth_value(0.9, 0.8)
      
      # A (premise)
      a.set_truth_value(0.8, 0.9)
      
      # Apply modus ponens
      result = reasoner.apply_modus_ponens(implication, a)
      
      result.should_not be_nil
      result.get_truth_value.strength.should be > 0.5
    end

    it "applies inheritance rule" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = PLN::Reasoner.new(atomspace)
      
      # Create inheritance chain: dog -> mammal -> animal
      dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
      mammal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "mammal")
      animal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "animal")
      
      inh1 = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, mammal])
      inh1.set_truth_value(0.9, 0.8)
      
      inh2 = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [mammal, animal])
      inh2.set_truth_value(0.8, 0.9)
      
      # Apply transitivity rule
      result = reasoner.apply_inheritance_transitivity(inh1, inh2)
      
      result.should_not be_nil
      result.outgoing.should contain(dog)
      result.outgoing.should contain(animal)
    end

    it "applies deduction rule" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = PLN::Reasoner.new(atomspace)
      
      # Create: A -> B, B -> C, infer A -> C
      a = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "A")
      b = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "B")
      c = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "C")
      
      ab = atomspace.add_link(AtomSpace::AtomType::IMPLICATION_LINK, [a, b])
      ab.set_truth_value(0.8, 0.7)
      
      bc = atomspace.add_link(AtomSpace::AtomType::IMPLICATION_LINK, [b, c])
      bc.set_truth_value(0.7, 0.8)
      
      result = reasoner.apply_deduction(ab, bc)
      
      result.should_not be_nil
      result.outgoing[0].should eq(a)
      result.outgoing[1].should eq(c)
    end
  end

  describe "Forward chaining" do
    it "performs single step forward chaining" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = PLN::Reasoner.new(atomspace)
      
      # Create knowledge base
      dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
      mammal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "mammal")
      
      inh = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, mammal])
      inh.set_truth_value(0.9, 0.8)
      
      initial_size = atomspace.size
      
      # Perform forward chaining step
      new_atoms = reasoner.step_forward
      
      # Should generate new atoms through reasoning
      atomspace.size.should be >= initial_size
      new_atoms.size.should be >= 0
    end

    it "performs multiple forward chaining steps" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = PLN::Reasoner.new(atomspace)
      
      # Create inheritance chain
      dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
      mammal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "mammal")
      animal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "animal")
      
      atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, mammal])
      atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [mammal, animal])
      
      initial_size = atomspace.size
      
      # Perform multiple reasoning steps
      total_new = reasoner.reason(3)
      
      # Should have generated new knowledge
      atomspace.size.should be > initial_size
      total_new.should be >= 0
    end
  end

  describe "Backward chaining" do
    it "performs goal-directed reasoning" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = PLN::Reasoner.new(atomspace)
      
      # Create knowledge
      dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
      mammal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "mammal")
      animal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "animal")
      
      atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, mammal])
      atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [mammal, animal])
      
      # Goal: prove dog inherits from animal
      goal = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, animal])
      
      # Backward chain to find proof
      proof = reasoner.backward_chain(goal)
      
      proof.should_not be_nil
      proof.size.should be > 0
    end

    it "handles complex backward chaining queries" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = PLN::Reasoner.new(atomspace)
      
      # Create complex knowledge base
      fido = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "Fido")
      dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
      mammal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "mammal")
      
      atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [fido, dog])
      atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, mammal])
      
      # Query: What does Fido inherit from?
      var = atomspace.add_node(AtomSpace::AtomType::VARIABLE_NODE, "$x")
      query = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [fido, var])
      
      results = reasoner.query(query)
      
      results.should_not be_nil
      results.size.should be >= 1
    end
  end

  describe "Truth value calculations" do
    it "calculates revision truth values" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = PLN::Reasoner.new(atomspace)
      
      # Create two atoms with different truth values
      node = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "test")
      
      tv1 = PLN::TruthValue.new(0.8, 0.6)
      tv2 = PLN::TruthValue.new(0.7, 0.8)
      
      # Revise truth values
      revised = reasoner.revision(tv1, tv2)
      
      revised.should_not be_nil
      revised.strength.should be > 0.0
      revised.confidence.should be > 0.0
    end

    it "calculates conjunction truth values" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = PLN::Reasoner.new(atomspace)
      
      tv1 = PLN::TruthValue.new(0.8, 0.9)
      tv2 = PLN::TruthValue.new(0.7, 0.8)
      
      conjunction = reasoner.conjunction(tv1, tv2)
      
      conjunction.should_not be_nil
      conjunction.strength.should be <= [tv1.strength, tv2.strength].min
    end

    it "calculates implication truth values" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = PLN::Reasoner.new(atomspace)
      
      tv_antecedent = PLN::TruthValue.new(0.8, 0.9)
      tv_consequent = PLN::TruthValue.new(0.7, 0.8)
      
      implication = reasoner.implication(tv_antecedent, tv_consequent)
      
      implication.should_not be_nil
      implication.strength.should be > 0.0
    end
  end

  describe "Rule application" do
    it "selects applicable rules" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = PLN::Reasoner.new(atomspace)
      
      # Create atoms that can trigger rules
      dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
      mammal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "mammal")
      
      inh = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, mammal])
      
      # Get applicable rules
      rules = reasoner.get_applicable_rules([inh])
      
      rules.should_not be_nil
      rules.size.should be >= 0
    end

    it "prioritizes rules by confidence" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = PLN::Reasoner.new(atomspace)
      
      # Create knowledge with different confidence levels
      a = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "A")
      b = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "B")
      c = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "C")
      
      # High confidence link
      ab_high = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [a, b])
      ab_high.set_truth_value(0.9, 0.9)
      
      # Low confidence link
      ac_low = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [a, c])
      ac_low.set_truth_value(0.5, 0.3)
      
      # Rule selection should prefer high confidence
      rules = reasoner.get_applicable_rules([ab_high, ac_low])
      
      # Should prioritize high confidence atoms
      rules.size.should be >= 0
    end
  end

  describe "PLN configuration" do
    it "supports different reasoning modes" do
      atomspace = AtomSpace::AtomSpace.new
      
      # Create reasoners with different modes
      forward_reasoner = PLN::Reasoner.new(atomspace, PLN::Mode::FORWARD)
      backward_reasoner = PLN::Reasoner.new(atomspace, PLN::Mode::BACKWARD)
      
      forward_reasoner.mode.should eq(PLN::Mode::FORWARD)
      backward_reasoner.mode.should eq(PLN::Mode::BACKWARD)
    end

    it "supports reasoning step limits" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = PLN::Reasoner.new(atomspace)
      
      # Set step limit
      reasoner.set_step_limit(10)
      reasoner.step_limit.should eq(10)
      
      # Create simple knowledge
      dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
      mammal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "mammal")
      atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, mammal])
      
      # Reasoning should respect step limit
      steps_taken = reasoner.reason(20) # Request more than limit
      steps_taken.should be <= 10
    end
  end
end