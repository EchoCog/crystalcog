# Crystal implementation of the OpenCog framework
#
# This is the main entry point for the Crystal OpenCog implementation.
# It provides a unified interface to all OpenCog components.

require "./cogutil/cogutil"
require "./atomspace/atomspace_main"
require "./opencog/opencog"
require "./cogserver/cogserver_main"
require "./pattern_matching/pattern_matching_main"
require "./attention/attention_main"
require "./moses/moses_main"

# Conditionally require server components
{% if flag?(:with_cogserver) %}
  require "./cogserver/cogserver"
{% end %}

{% if flag?(:with_tools) %}
  require "./tools/cogshell"
{% end %}

module CrystalCog
  VERSION = "0.1.0"
  
  # Initialize the OpenCog system
  def self.initialize
    CogUtil.initialize
    AtomSpace.initialize
    OpenCog.initialize
    CogServer.initialize
    PatternMatching.initialize
    Attention.initialize
    Moses.initialize
  end
  
  # Main entry point for command-line usage
  def self.main(args = ARGV)
    puts "CrystalCog #{VERSION} - OpenCog in Crystal"
    puts "Args received: #{args}"
    
    case args.first?
    when "server"
      puts "Starting CogServer..."
      CogServer.main(args[1..])
    when "shell"
      puts "CogShell functionality not yet implemented"
    when "test"
      puts "Running test AtomSpace operations..."
      test_basic_operations
    when "attention"
      puts "Running attention allocation demo..."
      test_attention_allocation
    when "moses"
      puts "Running MOSES evolutionary search..."
      Moses.main(args[1..])
    else
      puts "Usage: crystalcog [server|shell|test|attention|moses]"
      puts "  server     - Start the CogServer"
      puts "  shell      - Start interactive shell"  
      puts "  test       - Run basic test operations"
      puts "  attention  - Demo attention allocation mechanisms"
      puts "  moses      - Run MOSES evolutionary program learning"
    end
  end
  
  # Basic test operations to verify the system works
  private def self.test_basic_operations
    atomspace = AtomSpace::AtomSpace.new
    
    # Create some basic atoms
    node = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
    link = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [node, atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "animal")])
    
    puts "Created atom: #{node}"
    puts "Created link: #{link}"
    puts "AtomSpace size: #{atomspace.size}"
  end
  
  # Test attention allocation mechanisms
  private def self.test_attention_allocation
    puts "=== Attention Allocation Demo ==="
    
    # Create atomspace with some test atoms
    atomspace = AtomSpace::AtomSpace.new
    
    # Create a small knowledge graph
    dog = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "dog")
    mammal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "mammal")
    animal = atomspace.add_node(AtomSpace::AtomType::CONCEPT_NODE, "animal")
    
    # Create inheritance links
    dog_mammal = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [dog, mammal])
    mammal_animal = atomspace.add_link(AtomSpace::AtomType::INHERITANCE_LINK, [mammal, animal])
    
    puts "Created knowledge graph with #{atomspace.size} atoms"
    
    # Create attention allocation engine
    engine = Attention::AllocationEngine.new(atomspace)
    
    # Set some initial attention values
    engine.bank.stimulate(dog.handle, 100_i16)
    engine.bank.stimulate(mammal.handle, 50_i16)
    engine.bank.stimulate(dog_mammal.handle, 75_i16)
    
    puts "\nInitial attention values:"
    [dog, mammal, animal, dog_mammal, mammal_animal].each do |atom|
      av = engine.bank.get_attention_value(atom.handle)
      puts "  #{atom}: #{av || "none"}"
    end
    
    # Show initial statistics
    puts "\nInitial bank statistics:"
    engine.bank.get_statistics.each do |key, value|
      puts "  #{key}: #{value}"
    end
    
    # Set some goals for attention allocation
    goals = {
      Attention::Goal::Reasoning => 1.0,
      Attention::Goal::Learning => 0.8,
      Attention::Goal::Processing => 0.9
    }
    engine.set_goals(goals)
    
    puts "\nRunning attention allocation (3 cycles)..."
    results = engine.allocate_attention(3)
    
    puts "\nAllocation results:"
    results.each do |key, value|
      puts "  #{key}: #{value}"
    end
    
    puts "\nFinal attention values:"
    [dog, mammal, animal, dog_mammal, mammal_animal].each do |atom|
      av = engine.bank.get_attention_value(atom.handle)
      puts "  #{atom}: #{av || "none"}"
    end
    
    puts "\nFinal bank statistics:"
    engine.bank.get_statistics.each do |key, value|
      puts "  #{key}: #{value}"
    end
    
    # Show attentional focus
    puts "\nAttentional Focus (top #{engine.bank.attentional_focus.size} atoms):"
    engine.bank.attentional_focus.each_with_index do |handle, i|
      atom = atomspace.get_atom(handle)
      av = engine.bank.get_attention_value(handle)
      puts "  #{i + 1}. #{atom} - #{av}"
    end
    
    puts "\n=== Attention Allocation Demo Complete ==="
  end
end

# Run if this file is executed directly
if PROGRAM_NAME == __FILE__
  begin
    puts "Starting CrystalCog..."
    CrystalCog.main(ARGV)
    puts "Finished CrystalCog."
  rescue ex
    puts "Error: #{ex}"
    puts ex.backtrace.join("\n")
  end
end