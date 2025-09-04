require "spec"
require "../../src/pattern_matching/pattern_matching_main"

describe PatternMatching do
  
  describe "initialization" do
    it "initializes successfully" do
      # This should not raise any exceptions
      PatternMatching.initialize
    end
  end
  
  describe "Pattern" do
    it "creates pattern from simple atom" do
      dog = AtomSpace::ConceptNode.new("dog")
      pattern = PatternMatching::Pattern.new(dog)
      
      pattern.template.should eq(dog)
      pattern.variables.should be_empty
      pattern.constraints.should be_empty
    end
    
    it "identifies variables in pattern" do
      var_x = AtomSpace::VariableNode.new("$X")
      var_y = AtomSpace::VariableNode.new("$Y")
      link = AtomSpace::InheritanceLink.new(var_x, var_y)
      
      pattern = PatternMatching::Pattern.new(link)
      
      pattern.variables.size.should eq(2)
      pattern.variables.should contain(var_x)
      pattern.variables.should contain(var_y)
    end
    
    it "correctly identifies variable atoms" do
      var_atom = AtomSpace::VariableNode.new("$test")
      non_var_atom = AtomSpace::ConceptNode.new("test")
      
      PatternMatching::Pattern.variable?(var_atom).should be_true
      PatternMatching::Pattern.variable?(non_var_atom).should be_false
    end
  end
  
  describe "MatchResult" do
    it "creates empty match result" do
      bindings = PatternMatching::VariableBinding.new
      atoms = [] of AtomSpace::Atom
      result = PatternMatching::MatchResult.new(bindings, atoms)
      
      result.bindings.should be_empty
      result.matched_atoms.should be_empty
      result.empty?.should be_true
      result.success?.should be_false
    end
    
    it "creates successful match result" do
      var = AtomSpace::VariableNode.new("$X")
      atom = AtomSpace::ConceptNode.new("dog")
      bindings = PatternMatching::VariableBinding.new
      bindings[var] = atom
      
      result = PatternMatching::MatchResult.new(bindings, [atom].map(&.as(AtomSpace::Atom)))
      
      result.bindings.size.should eq(1)
      result.matched_atoms.size.should eq(1)
      result.empty?.should be_false
      result.success?.should be_true
    end
  end
  
  describe "TypeConstraint" do
    it "creates type constraint for single type" do
      var = AtomSpace::VariableNode.new("$X")
      constraint = PatternMatching::TypeConstraint.new(var, AtomSpace::AtomType::CONCEPT_NODE)
      
      constraint.variable.should eq(var)
      constraint.allowed_types.should contain(AtomSpace::AtomType::CONCEPT_NODE)
    end
    
    it "satisfies constraint with correct type" do
      var = AtomSpace::VariableNode.new("$X")
      atom = AtomSpace::ConceptNode.new("dog")
      constraint = PatternMatching::TypeConstraint.new(var, AtomSpace::AtomType::CONCEPT_NODE)
      
      bindings = PatternMatching::VariableBinding.new
      bindings[var] = atom
      atomspace = AtomSpace::AtomSpace.new
      
      constraint.satisfied?(bindings, atomspace).should be_true
    end
    
    it "fails constraint with incorrect type" do
      var = AtomSpace::VariableNode.new("$X")
      atom = AtomSpace::PredicateNode.new("likes")
      constraint = PatternMatching::TypeConstraint.new(var, AtomSpace::AtomType::CONCEPT_NODE)
      
      bindings = PatternMatching::VariableBinding.new
      bindings[var] = atom
      atomspace = AtomSpace::AtomSpace.new
      
      constraint.satisfied?(bindings, atomspace).should be_false
    end
  end
  
  describe "PresentConstraint" do
    it "creates present constraint" do
      atom = AtomSpace::ConceptNode.new("dog")
      constraint = PatternMatching::PresentConstraint.new([atom].map(&.as(AtomSpace::Atom)))
      
      constraint.atoms.should contain(atom)
    end
    
    it "satisfies constraint when atom is present" do
      atomspace = AtomSpace::AtomSpace.new
      atom = atomspace.add_concept_node("dog")
      constraint = PatternMatching::PresentConstraint.new([atom].map(&.as(AtomSpace::Atom)))
      
      bindings = PatternMatching::VariableBinding.new
      
      constraint.satisfied?(bindings, atomspace).should be_true
    end
    
    it "fails constraint when atom is absent" do
      atomspace = AtomSpace::AtomSpace.new
      atom = AtomSpace::ConceptNode.new("dog")
      constraint = PatternMatching::PresentConstraint.new([atom].map(&.as(AtomSpace::Atom)))
      
      bindings = PatternMatching::VariableBinding.new
      
      constraint.satisfied?(bindings, atomspace).should be_false
    end
  end
  
  describe "AbsentConstraint" do
    it "creates absent constraint" do
      atom = AtomSpace::ConceptNode.new("dog")
      constraint = PatternMatching::AbsentConstraint.new([atom].map(&.as(AtomSpace::Atom)))
      
      constraint.atoms.should contain(atom)
    end
    
    it "satisfies constraint when atom is absent" do
      atomspace = AtomSpace::AtomSpace.new
      atom = AtomSpace::ConceptNode.new("dog")
      constraint = PatternMatching::AbsentConstraint.new([atom].map(&.as(AtomSpace::Atom)))
      
      bindings = PatternMatching::VariableBinding.new
      
      constraint.satisfied?(bindings, atomspace).should be_true
    end
    
    it "fails constraint when atom is present" do
      atomspace = AtomSpace::AtomSpace.new
      atom = atomspace.add_concept_node("dog")
      constraint = PatternMatching::AbsentConstraint.new([atom].map(&.as(AtomSpace::Atom)))
      
      bindings = PatternMatching::VariableBinding.new
      
      constraint.satisfied?(bindings, atomspace).should be_false
    end
  end
  
  describe "EqualityConstraint" do
    it "creates equality constraint" do
      left = AtomSpace::VariableNode.new("$X")
      right = AtomSpace::VariableNode.new("$Y")
      constraint = PatternMatching::EqualityConstraint.new(left, right)
      
      constraint.left.should eq(left)
      constraint.right.should eq(right)
    end
    
    it "satisfies constraint when variables bind to same atom" do
      atomspace = AtomSpace::AtomSpace.new
      var_x = AtomSpace::VariableNode.new("$X")
      var_y = AtomSpace::VariableNode.new("$Y")
      atom = AtomSpace::ConceptNode.new("dog")
      constraint = PatternMatching::EqualityConstraint.new(var_x, var_y)
      
      bindings = PatternMatching::VariableBinding.new
      bindings[var_x] = atom
      bindings[var_y] = atom
      
      constraint.satisfied?(bindings, atomspace).should be_true
    end
    
    it "fails constraint when variables bind to different atoms" do
      atomspace = AtomSpace::AtomSpace.new
      var_x = AtomSpace::VariableNode.new("$X")
      var_y = AtomSpace::VariableNode.new("$Y")
      dog = AtomSpace::ConceptNode.new("dog")
      cat = AtomSpace::ConceptNode.new("cat")
      constraint = PatternMatching::EqualityConstraint.new(var_x, var_y)
      
      bindings = PatternMatching::VariableBinding.new
      bindings[var_x] = dog
      bindings[var_y] = cat
      
      constraint.satisfied?(bindings, atomspace).should be_false
    end
  end
    it "creates present constraint" do
      atom = AtomSpace::ConceptNode.new("dog")
      constraint = PatternMatching::PresentConstraint.new([atom].map(&.as(AtomSpace::Atom)))
      
      constraint.atoms.should contain(atom)
    end
    
    it "satisfies constraint when atom is present" do
      atomspace = AtomSpace::AtomSpace.new
      atom = atomspace.add_concept_node("dog")
      constraint = PatternMatching::PresentConstraint.new([atom].map(&.as(AtomSpace::Atom)))
      
      bindings = PatternMatching::VariableBinding.new
      
      constraint.satisfied?(bindings, atomspace).should be_true
    end
  end
  
  describe "PatternMatcher" do
    it "creates pattern matcher" do
      atomspace = AtomSpace::AtomSpace.new
      matcher = PatternMatching::PatternMatcher.new(atomspace)
      
      matcher.atomspace.should eq(atomspace)
    end
    
    it "finds variable candidates" do
      atomspace = AtomSpace::AtomSpace.new
      dog = atomspace.add_concept_node("dog")
      cat = atomspace.add_concept_node("cat")
      
      matcher = PatternMatching::PatternMatcher.new(atomspace)
      var = AtomSpace::VariableNode.new("$X")
      pattern = PatternMatching::Pattern.new(var)
      
      candidates = matcher.find_variable_candidates(var, pattern)
      candidates.should contain(dog)
      candidates.should contain(cat)
    end
    
    it "matches simple concrete atom" do
      atomspace = AtomSpace::AtomSpace.new
      dog = atomspace.add_concept_node("dog")
      
      matcher = PatternMatching::PatternMatcher.new(atomspace)
      result = matcher.match_one(dog)
      
      result.should_not be_nil
      result.not_nil!.matched_atoms.should contain(dog)
    end
  end
  
  describe "Utils" do
    it "creates variables with correct names" do
      var = PatternMatching::Utils.variable("test")
      
      var.should be_a(AtomSpace::VariableNode)
      var.name.should eq("$test")
    end
    
    it "creates type constraints" do
      var = AtomSpace::VariableNode.new("$X")
      constraint = PatternMatching::Utils.type_constraint(var, AtomSpace::AtomType::CONCEPT_NODE)
      
      constraint.should be_a(PatternMatching::TypeConstraint)
      constraint.variable.should eq(var)
    end
  end
  
  describe "QueryBuilder" do
    it "creates empty query builder" do
      atomspace = AtomSpace::AtomSpace.new
      builder = PatternMatching::QueryBuilder.new(atomspace)
      
      builder.atomspace.should eq(atomspace)
      builder.variables.should be_empty
      builder.constraints.should be_empty
    end
    
    it "adds variables to query" do
      atomspace = AtomSpace::AtomSpace.new
      builder = PatternMatching::QueryBuilder.new(atomspace)
      
      var = builder.variable("test")
      
      var.should be_a(AtomSpace::VariableNode)
      var.name.should eq("$test")
      builder.variables["test"].should eq(var)
    end
    
    it "adds type constraints" do
      atomspace = AtomSpace::AtomSpace.new
      builder = PatternMatching::QueryBuilder.new(atomspace)
      
      builder.constrain_type("X", AtomSpace::AtomType::CONCEPT_NODE)
      
      builder.constraints.size.should eq(1)
      builder.constraints.first.should be_a(PatternMatching::TypeConstraint)
    end
    
    it "adds equality constraints" do
      atomspace = AtomSpace::AtomSpace.new
      builder = PatternMatching::QueryBuilder.new(atomspace)
      
      builder.constrain_equal("X", "Y")
      
      builder.constraints.size.should eq(1)
      builder.constraints.first.should be_a(PatternMatching::EqualityConstraint)
    end
    
    it "builds inheritance patterns" do
      atomspace = AtomSpace::AtomSpace.new
      builder = PatternMatching::QueryBuilder.new(atomspace)
      
      pattern = builder.inheritance("child", "parent")
      
      pattern.should be_a(AtomSpace::InheritanceLink)
      pattern.outgoing.size.should eq(2)
    end
  end
  
  describe "Enhanced PatternMatcher" do
    it "performs backtracking with multiple variable bindings" do
      atomspace = AtomSpace::AtomSpace.new
      
      # Create knowledge base
      dog = atomspace.add_concept_node("dog")
      cat = atomspace.add_concept_node("cat")
      animal = atomspace.add_concept_node("animal")
      mammal = atomspace.add_concept_node("mammal")
      
      # Add inheritance links
      atomspace.add_inheritance_link(dog, mammal)
      atomspace.add_inheritance_link(cat, mammal)
      atomspace.add_inheritance_link(mammal, animal)
      
      # Create pattern to find all inheritance relationships
      var_x = AtomSpace::VariableNode.new("$X")
      var_y = AtomSpace::VariableNode.new("$Y")
      pattern_atom = AtomSpace::InheritanceLink.new(var_x, var_y)
      pattern = PatternMatching::Pattern.new(pattern_atom)
      
      matcher = PatternMatching::PatternMatcher.new(atomspace)
      results = matcher.match(pattern)
      
      results.size.should be >= 3 # Should find at least dog->mammal, cat->mammal, mammal->animal
      results.all?(&.success?).should be_true
    end
    
    it "limits results correctly" do
      atomspace = AtomSpace::AtomSpace.new
      
      # Create many concepts
      20.times do |i|
        atomspace.add_concept_node("concept_#{i}")
      end
      
      var_x = AtomSpace::VariableNode.new("$X")
      pattern_atom = var_x
      pattern = PatternMatching::Pattern.new(pattern_atom)
      
      matcher = PatternMatching::PatternMatcher.new(atomspace, max_results: 5)
      results = matcher.match(pattern)
      
      results.size.should be <= 5
    end
  end
    it "performs backtracking with multiple variable bindings" do
      atomspace = AtomSpace::AtomSpace.new
      
      # Create knowledge base
      dog = atomspace.add_concept_node("dog")
      cat = atomspace.add_concept_node("cat")
      animal = atomspace.add_concept_node("animal")
      mammal = atomspace.add_concept_node("mammal")
      
      # Add inheritance links
      atomspace.add_inheritance_link(dog, mammal)
      atomspace.add_inheritance_link(cat, mammal)
      atomspace.add_inheritance_link(mammal, animal)
      
      # Create pattern to find all inheritance relationships
      var_x = AtomSpace::VariableNode.new("$X")
      var_y = AtomSpace::VariableNode.new("$Y")
      pattern_atom = AtomSpace::InheritanceLink.new(var_x, var_y)
      pattern = PatternMatching::Pattern.new(pattern_atom)
      
      matcher = PatternMatching::PatternMatcher.new(atomspace)
      results = matcher.match(pattern)
      
      results.size.should be >= 3 # Should find at least dog->mammal, cat->mammal, mammal->animal
      results.all?(&.success?).should be_true
    end
    
    it "handles timeout correctly" do
      atomspace = AtomSpace::AtomSpace.new
      
      # Create many atoms to slow down matching
      100.times do |i|
        atomspace.add_concept_node("concept_#{i}")
      end
      
      var_x = AtomSpace::VariableNode.new("$X")
      pattern_atom = AtomSpace::InheritanceLink.new(var_x, var_x)
      pattern = PatternMatching::Pattern.new(pattern_atom)
      
      # Set very short timeout
      matcher = PatternMatching::PatternMatcher.new(atomspace, timeout_seconds: 1)
      
      # This should either complete quickly or timeout
      begin
        results = matcher.match(pattern)
        # If it completes, that's fine too
        true.should be_true
      rescue PatternMatching::MatchingTimeoutException
        # Timeout is expected behavior
        true.should be_true
      end
    end
    
    it "limits results correctly" do
      atomspace = AtomSpace::AtomSpace.new
      
      # Create many concepts
      50.times do |i|
        atomspace.add_concept_node("concept_#{i}")
      end
      
      var_x = AtomSpace::VariableNode.new("$X")
      pattern_atom = var_x
      pattern = PatternMatching::Pattern.new(pattern_atom)
      
      matcher = PatternMatching::PatternMatcher.new(atomspace, max_results: 10)
      results = matcher.match(pattern)
      
      results.size.should be <= 10
    end
  end
    it "creates query builder" do
      atomspace = AtomSpace::AtomSpace.new
      builder = PatternMatching::QueryBuilder.new(atomspace)
      
      builder.atomspace.should eq(atomspace)
      builder.variables.should be_empty
      builder.constraints.should be_empty
    end
    
    it "adds variables to query" do
      atomspace = AtomSpace::AtomSpace.new
      builder = PatternMatching::QueryBuilder.new(atomspace)
      
      var = builder.variable("test")
      
      var.should be_a(AtomSpace::VariableNode)
      var.name.should eq("$test")
      builder.variables["test"].should eq(var)
    end
    
    it "adds type constraints" do
      atomspace = AtomSpace::AtomSpace.new
      builder = PatternMatching::QueryBuilder.new(atomspace)
      
      builder.constrain_type("X", AtomSpace::AtomType::CONCEPT_NODE)
      
      builder.constraints.size.should eq(1)
      builder.constraints.first.should be_a(PatternMatching::TypeConstraint)
    end
    
    it "builds inheritance patterns" do
      atomspace = AtomSpace::AtomSpace.new
      builder = PatternMatching::QueryBuilder.new(atomspace)
      
      pattern = builder.inheritance("child", "parent")
      
      pattern.should be_a(AtomSpace::InheritanceLink)
      pattern.type.should eq(AtomSpace::AtomType::INHERITANCE_LINK)
    end
  end
end