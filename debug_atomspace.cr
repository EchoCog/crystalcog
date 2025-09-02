require "./src/atomspace/atomspace_main"

atomspace = AtomSpace::AtomSpace.new
dog = atomspace.add_concept_node("dog")
cat = atomspace.add_concept_node("cat")

puts "Total atoms: #{atomspace.size}"
puts "All atoms: #{atomspace.get_atoms_by_type(AtomSpace::AtomType::ATOM).size}"
puts "All nodes: #{atomspace.get_atoms_by_type(AtomSpace::AtomType::NODE).size}"
puts "Concept nodes: #{atomspace.get_atoms_by_type(AtomSpace::AtomType::CONCEPT_NODE).size}"

# Check specific types
puts "Dog type: #{dog.type}"
puts "Cat type: #{cat.type}"

# Try getting atoms of specific types
concept_nodes = atomspace.get_atoms_by_type(AtomSpace::AtomType::CONCEPT_NODE)
puts "Found concept nodes: #{concept_nodes.size}"
concept_nodes.each_with_index do |atom, i|
  puts "  #{i}: #{atom}"
end