# Crystal implementation of the OpenCog framework
#
# This is the main entry point for the Crystal OpenCog implementation.
# It provides a unified interface to all OpenCog components.

require "./cogutil/cogutil"
require "./atomspace/atomspace"  
require "./opencog/opencog"

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
    
    case args.first?
    when "server"
      require "./cogserver/cogserver"
      CogServer.start
    when "shell"
      require "./tools/cogshell"
      CogShell.start
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
    atomspace = AtomSpace.new
    
    # Create some basic atoms
    node = atomspace.add_node("ConceptNode", "dog")
    link = atomspace.add_link("InheritanceLink", [node, atomspace.add_node("ConceptNode", "animal")])
    
    puts "Created atom: #{node}"
    puts "Created link: #{link}"
    puts "AtomSpace size: #{atomspace.size}"
  end
end

# Run if this file is executed directly
if PROGRAM_NAME == __FILE__
  CrystalCog.main
end