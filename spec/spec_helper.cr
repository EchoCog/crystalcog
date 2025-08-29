require "spec"

# Require all cogutil specs
require "./cogutil/logger_spec"
require "./cogutil/config_spec" 
require "./cogutil/randgen_spec"

# Require all atomspace specs
require "./atomspace/truthvalue_spec"
require "./atomspace/atom_spec"
require "./atomspace/atomspace_spec"

# Main spec runner for all tests
describe "CrystalCog Integration Tests" do
  it "initializes systems correctly" do
    # Test that modules can be initialized without errors
    CogUtil.initialize
    AtomSpace.initialize
    
    # Basic functionality test
    atomspace = AtomSpace.new_atomspace
    dog = atomspace.add_concept_node("dog")
    animal = atomspace.add_concept_node("animal")
    inheritance = atomspace.add_inheritance_link(dog, animal)
    
    atomspace.size.should eq(3)
    atomspace.node_count.should eq(2)
    atomspace.link_count.should eq(1)
    
    # Clean up
    atomspace.clear
  end
end