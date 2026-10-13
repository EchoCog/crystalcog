require "../opencog/opencog"

module CrystalCog
  # A small local shell over a persistent in-session knowledge graph.
  class Shell
    getter atomspace : AtomSpace::AtomSpace

    def initialize(@atomspace = AtomSpace::AtomSpace.new, @input : IO = STDIN, @output : IO = STDOUT)
    end

    def run
      @output.puts "CogShell - type help for commands"
      loop do
        @output.print "cog> "
        @output.flush
        line = @input.gets
        break unless line
        break unless execute(line.strip)
      end
    end

    def execute(line : String) : Bool
      command, _, argument = line.partition(' ')
      argument = argument.strip
      case command
      when "", "#"
      when "help"
        @output.puts "Commands: add NAME | list | stats | reason | help | quit"
      when "add"
        if argument.empty?
          @output.puts "Usage: add NAME"
        else
          atom = @atomspace.add_concept_node(argument)
          @output.puts "Added #{atom.handle}: #{atom}"
        end
      when "list"
        @atomspace.get_all_atoms.each { |atom| @output.puts "#{atom.handle}: #{atom}" }
      when "stats"
        @output.puts "Atoms: #{@atomspace.size}, nodes: #{@atomspace.node_count}, links: #{@atomspace.link_count}"
      when "reason"
        results = PLN.create_engine(@atomspace).reason(10)
        @output.puts "Derived #{results.size} inferences"
      when "quit", "exit"
        return false
      else
        @output.puts "Unknown command: #{command}. Type help for commands."
      end
      true
    end
  end
end
