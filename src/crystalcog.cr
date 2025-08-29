# Crystal implementation of the OpenCog framework
#
# This is the main entry point for the Crystal OpenCog implementation.
# It provides a unified interface to all OpenCog components.

require "./cogutil/cogutil"
require "./atomspace/atomspace_main"
require "./opencog/opencog"

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
  end
  
  # Main entry point for command-line usage
  def self.main(args = ARGV)
    puts "CrystalCog #{VERSION} - OpenCog in Crystal"
    puts "Args received: #{args}"
    
    case args.first?
    when "server"
      puts "CogServer functionality not yet implemented"
    when "shell"
      puts "CogShell functionality not yet implemented"
    when "test"
      puts "Running test AtomSpace operations..."
      test_basic_operations
    else
      puts "Usage: crystalcog [server|shell|test]"
      puts "  server - Start the CogServer"
      puts "  shell  - Start interactive shell"  
      puts "  test   - Run basic test operations"
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
end

# Run if this file is executed directly
if PROGRAM_NAME == __FILE__
  begin
    puts "Starting CrystalCog..."
    CrystalCog.main
    puts "Finished CrystalCog."
  rescue ex
    puts "Error: #{ex}"
    puts ex.backtrace.join("\n")
  end
end