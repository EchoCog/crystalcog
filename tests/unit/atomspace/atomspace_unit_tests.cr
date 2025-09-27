require "spec"
require "../../../src/cogutil/cogutil"
require "../../../src/atomspace/atomspace_main"

# Unit tests for AtomSpace functionality
describe AtomSpace do
  before_each do
    CogUtil.initialize
    AtomSpace.initialize
  end

  describe "AtomSpace creation" do
    it "creates an empty atomspace" do
      atomspace = AtomSpace::AtomSpace.new
      atomspace.should_not be_nil
      atomspace.size.should eq(0)
    end

    it "supports multiple atomspaces" do
      as1 = AtomSpace::AtomSpace.new
      as2 = AtomSpace::AtomSpace.new
      
      as1.should_not be_nil
      as2.should_not be_nil
      as1.should_not eq(as2)
    end
  end

  describe "Node operations" do
    it "creates concept nodes" do
      atomspace = AtomSpace::AtomSpace.new
      node = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "test")
      
      node.should_not be_nil
      atomspace.size.should eq(1)
      atomspace.contains?(node).should be_true
    end

    it "creates variable nodes" do
      atomspace = AtomSpace::AtomSpace.new
      var = atomspace.add_node(AtomSpace::AtomType::VARIABLE_NODE, "$x")
      
      var.should_not be_nil
      atomspace.size.should eq(1)
    end

    it "handles duplicate nodes correctly" do
      atomspace = AtomSpace::AtomSpace.new
      node1 = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "test")
      node2 = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "test")
      
      atomspace.size.should eq(1) # Should not create duplicates
      node1.should eq(node2)
    end
  end

  describe "Link operations" do
    it "creates inheritance links" do
      atomspace = AtomSpace::AtomSpace.new
      dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
      mammal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "mammal")
      
      link = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, mammal])
      
      link.should_not be_nil
      atomspace.size.should eq(3) # 2 nodes + 1 link
      atomspace.contains?(link).should be_true
    end

    it "creates similarity links" do
      atomspace = AtomSpace::AtomSpace.new
      cat = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "cat")
      dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
      
      link = atomspace.add_link(AtomSpace::AtomType::SIMILARITY_LINK, [cat, dog])
      
      link.should_not be_nil
      atomspace.size.should eq(3)
    end

    it "handles links with multiple atoms" do
      atomspace = AtomSpace::AtomSpace.new
      atoms = (1..5).map do |i|
        atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "node#{i}")
      end
      
      link = atomspace.add_link(AtomSpace::AtomType::LIST_LINK, atoms)
      
      link.should_not be_nil
      atomspace.size.should eq(6) # 5 nodes + 1 link
    end
  end

  describe "Query operations" do
    it "finds atoms by type" do
      atomspace = AtomSpace::AtomSpace.new
      dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
      var = atomspace.add_node(AtomSpace::AtomType::VARIABLE_NODE, "$x")
      
      concepts = atomspace.get_atoms_by_type(AtomSpace::AtomType::CONCEPT_NODE)
      variables = atomspace.get_atoms_by_type(AtomSpace::AtomType::VARIABLE_NODE)
      
      concepts.size.should eq(1)
      variables.size.should eq(1)
      concepts.should contain(dog)
      variables.should contain(var)
    end

    it "finds atoms by name" do
      atomspace = AtomSpace::AtomSpace.new
      dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
      
      found = atomspace.get_atoms_by_name("dog")
      found.size.should eq(1)
      found.should contain(dog)
    end
  end

  describe "Truth values" do
    it "supports strength and confidence values" do
      atomspace = AtomSpace::AtomSpace.new
      node = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "test")
      
      # Set truth value
      node.set_truth_value(0.8, 0.9)
      
      tv = node.get_truth_value
      tv.should_not be_nil
      tv.strength.should be_close(0.8, 0.01)
      tv.confidence.should be_close(0.9, 0.01)
    end

    it "supports different truth value types" do
      atomspace = AtomSpace::AtomSpace.new
      node = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "test")
      
      # Simple truth value
      node.set_simple_tv(0.7, 0.8)
      tv = node.get_truth_value
      tv.strength.should be_close(0.7, 0.01)
      
      # Count truth value  
      node.set_count_tv(0.6, 0.5, 100)
      tv = node.get_truth_value
      tv.strength.should be_close(0.6, 0.01)
    end
  end

  describe "AtomSpace persistence" do
    it "supports clear operation" do
      atomspace = AtomSpace::AtomSpace.new
      atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "test1")
      atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "test2")
      
      atomspace.size.should eq(2)
      atomspace.clear
      atomspace.size.should eq(0)
    end

    it "supports atom removal" do
      atomspace = AtomSpace::AtomSpace.new
      node = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "test")
      
      atomspace.size.should eq(1)
      result = atomspace.remove_atom(node)
      result.should be_true
      atomspace.size.should eq(0)
    end
  end

  describe "AtomSpace statistics" do
    it "provides accurate size counts" do
      atomspace = AtomSpace::AtomSpace.new
      
      # Add nodes
      3.times do |i|
        atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "node#{i}")
      end
      
      # Add link
      nodes = atomspace.get_atoms_by_type(AtomSpace::AtomType::CONCEPT_NODE)
      atomspace.add_link(AtomSpace::AtomType::LIST_LINK, nodes)
      
      atomspace.size.should eq(4) # 3 nodes + 1 link
      atomspace.node_count.should eq(3)
      atomspace.link_count.should eq(1)
    end

    it "provides type statistics" do
      atomspace = AtomSpace::AtomSpace.new
      
      atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
      atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "cat")
      atomspace.add_node(AtomSpace::AtomType::VARIABLE_NODE, "$x")
      
      stats = atomspace.get_type_statistics
      stats[AtomSpace::AtomType::CONCEPT_NODE].should eq(2)
      stats[AtomSpace::AtomType::VARIABLE_NODE].should eq(1)
    end
  end
end