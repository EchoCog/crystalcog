#!/usr/bin/env crystal

# Basic test of the CrystalCog system
require "./src/cogutil/cogutil"
require "./src/atomspace/atomspace_main"

puts "Testing CrystalCog basic functionality..."

# Initialize the systems
CogUtil.initialize
AtomSpace.initialize

# Create an AtomSpace
atomspace = AtomSpace::AtomSpace.new

# Create some basic atoms
puts "Creating atoms..."
dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
animal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "animal")
inheritance = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, animal])

puts "Created atoms:"
puts "  Dog: #{dog}"
puts "  Animal: #{animal}"
puts "  Inheritance: #{inheritance}"
puts "AtomSpace size: #{atomspace.size}"

# Test AtomSpace operations
puts "\nTesting AtomSpace operations..."
puts "Contains dog? #{atomspace.contains?(dog)}"
puts "Contains animal? #{atomspace.contains?(animal)}"

# Get all concept nodes
concept_nodes = atomspace.get_atoms_by_type(AtomSpace::AtomType::CONCEPT_NODE)
puts "Concept nodes: #{concept_nodes.size}"

puts "\nCrystalCog basic test completed successfully!"