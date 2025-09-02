require "../spec_helper"
require "../../src/opencog/opencog"

describe OpenCog::Reasoning do
  describe ".infer" do
    it "generates inferences using PLN and URE" do
      atomspace = AtomSpace::AtomSpace.new
      
      # Add some test atoms
      cat = atomspace.add_concept_node("Cat")
      animal = atomspace.add_concept_node("Animal")
      mammal = atomspace.add_concept_node("Mammal")
      
      # Add relationships
      atomspace.add_inheritance_link(cat, mammal, AtomSpace::SimpleTruthValue.new(0.9, 0.8))
      atomspace.add_inheritance_link(mammal, animal, AtomSpace::SimpleTruthValue.new(0.95, 0.9))
      
      # Run inference
      results = OpenCog::Reasoning.infer(atomspace, 5)
      
      results.should be_a(Array(AtomSpace::Atom))
      results.size.should be > 0
    end
  end
  
  describe ".can_conclude?" do
    it "determines if a conclusion can be reached" do
      atomspace = AtomSpace::AtomSpace.new
      
      # Create a simple implication
      cat = atomspace.add_concept_node("Cat")
      animal = atomspace.add_concept_node("Animal")
      goal = atomspace.add_inheritance_link(cat, animal)
      
      # Should be true if the goal already exists
      result = OpenCog::Reasoning.can_conclude?(atomspace, goal)
      result.should be_true
    end
  end
  
  describe ".similarity" do
    it "calculates similarity between atoms" do
      atomspace = AtomSpace::AtomSpace.new
      
      cat = atomspace.add_concept_node("Cat")
      dog = atomspace.add_concept_node("Dog")
      animal = atomspace.add_concept_node("Animal")
      
      # Both inherit from animal
      atomspace.add_inheritance_link(cat, animal)
      atomspace.add_inheritance_link(dog, animal)
      
      similarity = OpenCog::Reasoning.similarity(atomspace, cat, dog)
      similarity.should be > 0.0
      similarity.should be <= 1.0
    end
  end
end

describe OpenCog::AtomUtils do
  describe ".create_hierarchy" do
    it "creates concept hierarchy" do
      atomspace = AtomSpace::AtomSpace.new
      
      hierarchy = {
        "Cat" => ["Mammal", "Pet"],
        "Dog" => ["Mammal", "Pet"],
        "Mammal" => ["Animal"]
      }
      
      created = OpenCog::AtomUtils.create_hierarchy(atomspace, hierarchy)
      created.should be_a(Array(AtomSpace::Atom))
      created.size.should be > 0
      
      # Check that concepts exist
      atomspace.get_nodes_by_name("Cat").size.should eq(1)
      atomspace.get_nodes_by_name("Mammal").size.should eq(1)
    end
  end
  
  describe ".create_semantic_network" do
    it "creates semantic network from facts" do
      atomspace = AtomSpace::AtomSpace.new
      
      facts = [
        {"subject" => "Cat", "predicate" => "likes", "object" => "Fish"},
        {"subject" => "Dog", "predicate" => "likes", "object" => "Bone"}
      ]
      
      created = OpenCog::AtomUtils.create_semantic_network(atomspace, facts)
      created.should be_a(Array(AtomSpace::Atom))
      created.size.should be > 0
      
      # Check that nodes were created
      atomspace.get_nodes_by_name("Cat").size.should eq(1)
      atomspace.get_nodes_by_name("likes").size.should eq(1)
    end
  end
  
  describe ".extract_subgraph" do
    it "extracts subgraph around an atom" do
      atomspace = AtomSpace::AtomSpace.new
      
      cat = atomspace.add_concept_node("Cat")
      animal = atomspace.add_concept_node("Animal")
      inheritance = atomspace.add_inheritance_link(cat, animal)
      
      subgraph = OpenCog::AtomUtils.extract_subgraph(atomspace, cat, 2)
      subgraph.should contain(cat)
      subgraph.should contain(inheritance)
    end
  end
  
  describe ".merge_atoms" do
    it "merges two identical atoms" do
      atomspace = AtomSpace::AtomSpace.new
      
      atom1 = atomspace.add_concept_node("Cat", AtomSpace::SimpleTruthValue.new(0.8, 0.7))
      atom2 = AtomSpace::Node.new(AtomSpace::AtomType::CONCEPT_NODE, "Cat", AtomSpace::SimpleTruthValue.new(0.9, 0.8))
      
      merged = OpenCog::AtomUtils.merge_atoms(atomspace, atom1, atom2)
      merged.should be_a(AtomSpace::Atom)
    end
  end
end

describe OpenCog::Query do
  describe "QueryResult" do
    it "creates query result with bindings" do
      bindings = {"X" => AtomSpace::Node.new(AtomSpace::AtomType::CONCEPT_NODE, "Cat")}
      result = OpenCog::Query::QueryResult.new(bindings, 0.8)
      
      result.bindings.should eq(bindings)
      result.confidence.should eq(0.8)
    end
  end
  
  describe ".query_pattern" do
    it "executes pattern query" do
      atomspace = AtomSpace::AtomSpace.new
      
      cat = atomspace.add_concept_node("Cat")
      pattern = cat  # Simple pattern matching the cat node
      
      results = OpenCog::Query.query_pattern(atomspace, pattern)
      results.should be_a(Array(OpenCog::Query::QueryResult))
    end
  end
  
  describe ".find_instances" do
    it "finds instances of a concept" do
      atomspace = AtomSpace::AtomSpace.new
      
      cat = atomspace.add_concept_node("Cat")
      animal = atomspace.add_concept_node("Animal")
      atomspace.add_inheritance_link(cat, animal)
      
      instances = OpenCog::Query.find_instances(atomspace, animal)
      instances.should contain(cat)
    end
  end
  
  describe ".find_predicates" do
    it "finds predicates applied to a subject" do
      atomspace = AtomSpace::AtomSpace.new
      
      cat = atomspace.add_concept_node("Cat")
      likes = atomspace.add_predicate_node("likes")
      fish = atomspace.add_concept_node("Fish")
      
      args = atomspace.add_list_link([cat, fish])
      atomspace.add_evaluation_link(likes, args)
      
      predicates = OpenCog::Query.find_predicates(atomspace, cat)
      predicates.keys.should contain(likes)
    end
  end
end

describe OpenCog::Learning do
  describe ".learn_implications" do
    it "learns implications from patterns" do
      atomspace = AtomSpace::AtomSpace.new
      
      # Create some evaluation patterns
      cat = atomspace.add_concept_node("Cat")
      likes = atomspace.add_predicate_node("likes")
      sleeps = atomspace.add_predicate_node("sleeps")
      fish = atomspace.add_concept_node("Fish")
      
      args1 = atomspace.add_list_link([cat])
      args2 = atomspace.add_list_link([cat, fish])
      
      atomspace.add_evaluation_link(likes, args2)
      atomspace.add_evaluation_link(sleeps, args1)
      
      implications = OpenCog::Learning.learn_implications(atomspace)
      implications.should be_a(Array(AtomSpace::Atom))
    end
  end
end

describe OpenCog::OpenCogReasoner do
  describe "#reason" do
    it "performs comprehensive reasoning" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = OpenCog.create_reasoner(atomspace)
      
      # Add some test knowledge
      cat = atomspace.add_concept_node("Cat")
      animal = atomspace.add_concept_node("Animal")
      atomspace.add_inheritance_link(cat, animal)
      
      results = reasoner.reason(3)
      results.should be_a(Array(AtomSpace::Atom))
    end
  end
  
  describe "#query" do
    it "queries the knowledge base" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = OpenCog.create_reasoner(atomspace)
      
      cat = atomspace.add_concept_node("Cat")
      results = reasoner.query(cat)
      
      results.should be_a(Array(OpenCog::Query::QueryResult))
    end
  end
  
  describe "#can_achieve?" do
    it "checks if goal can be achieved" do
      atomspace = AtomSpace::AtomSpace.new
      reasoner = OpenCog.create_reasoner(atomspace)
      
      goal = atomspace.add_concept_node("TestGoal")
      result = reasoner.can_achieve?(goal)
      
      result.should be_a(Bool)
    end
  end
end