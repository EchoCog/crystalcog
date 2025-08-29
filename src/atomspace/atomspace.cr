# Crystal implementation of OpenCog AtomSpace
# Converted from atomspace/opencog/atomspace/AtomSpace.h and AtomSpace.cc
#
# The AtomSpace is the central knowledge store for OpenCog.

require "./truthvalue"
require "./atom"
require "../cogutil/cogutil"

module AtomSpace
  # Events emitted by AtomSpace for observers
  enum AtomSpaceEvent
    ATOM_ADDED
    ATOM_REMOVED
    TRUTH_VALUE_CHANGED
    ATTENTION_VALUE_CHANGED
  end
  
  # Signal for AtomSpace events
  alias AtomSpaceSignal = Proc(AtomSpaceEvent, Atom, Void)
  
  # The AtomSpace - central knowledge repository
  class AtomSpace
    # Atom storage - hash maps for efficient lookup
    @atoms_by_handle : Hash(Handle, Atom)
    @atoms_by_type : Hash(AtomType, Set(Atom))
    @atoms_by_name : Hash(String, Set(Atom)) # For nodes
    @atoms_by_outgoing : Hash(Array(Handle), Set(Atom)) # For links
    
    # Event system
    @observers : Array(AtomSpaceSignal)
    
    # Thread safety
    @mutex : Mutex
    
    # Statistics
    @atom_count : UInt64
    @node_count : UInt64
    @link_count : UInt64
    
    # Unique instance counter
    @@instance_count = 0
    
    def initialize
      @atoms_by_handle = Hash(Handle, Atom).new
      @atoms_by_type = Hash(AtomType, Set(Atom)).new
      @atoms_by_name = Hash(String, Set(Atom)).new
      @atoms_by_outgoing = Hash(Array(Handle), Set(Atom)).new
      @observers = Array(AtomSpaceSignal).new
      @mutex = Mutex.new
      @atom_count = 0_u64
      @node_count = 0_u64
      @link_count = 0_u64
      
      @@instance_count += 1
      CogUtil::Logger.info("AtomSpace #{@@instance_count} created")
    end
    
    # Add an atom to the AtomSpace
    def add_atom(atom : Atom) : Atom
      @mutex.synchronize do
        # Check if atom already exists
        existing = find_existing_atom(atom)
        if existing
          # Merge truth values if different
          if existing.truth_value != atom.truth_value
            merged_tv = existing.truth_value.merge(atom.truth_value)
            existing.truth_value = merged_tv
            emit_event(AtomSpaceEvent::TRUTH_VALUE_CHANGED, existing)
          end
          return existing
        end
        
        # Add new atom
        @atoms_by_handle[atom.handle] = atom
        
        # Index by type
        type_set = @atoms_by_type[atom.type]? || Set(Atom).new
        type_set.add(atom)
        @atoms_by_type[atom.type] = type_set
        
        # Index nodes by name
        if atom.is_a?(Node)
          name_set = @atoms_by_name[atom.name]? || Set(Atom).new
          name_set.add(atom)
          @atoms_by_name[atom.name] = name_set
        end
        
        # Index links by outgoing set
        if atom.is_a?(Link)
          outgoing_handles = atom.outgoing.map(&.handle)
          outgoing_set = @atoms_by_outgoing[outgoing_handles]? || Set(Atom).new
          outgoing_set.add(atom)
          @atoms_by_outgoing[outgoing_handles] = outgoing_set
        end
        
        # Update statistics
        @atom_count += 1
        if atom.node?
          @node_count += 1
        else
          @link_count += 1
        end
        
        emit_event(AtomSpaceEvent::ATOM_ADDED, atom)
        atom
      end
    end
    
    # Find existing atom with same content
    private def find_existing_atom(atom : Atom) : Atom?
      case atom
      when Node
        name_set = @atoms_by_name[atom.name]?
        return nil unless name_set
        
        name_set.find { |existing| existing.type == atom.type }
      when Link
        outgoing_handles = atom.outgoing.map(&.handle)
        outgoing_set = @atoms_by_outgoing[outgoing_handles]?
        return nil unless outgoing_set
        
        outgoing_set.find { |existing| existing.type == atom.type }
      else
        nil
      end
    end
    
    # Remove atom from AtomSpace
    def remove_atom(atom : Atom) : Bool
      @mutex.synchronize do
        return false unless @atoms_by_handle.has_key?(atom.handle)
        
        # Check if atom has incoming links
        if has_incoming_links?(atom)
          CogUtil::Logger.warn("Cannot remove atom with incoming links: #{atom}")
          return false
        end
        
        # Remove from indices
        @atoms_by_handle.delete(atom.handle)
        
        if type_set = @atoms_by_type[atom.type]?
          type_set.delete(atom)
          @atoms_by_type.delete(atom.type) if type_set.empty?
        end
        
        if atom.is_a?(Node)
          if name_set = @atoms_by_name[atom.name]?
            name_set.delete(atom)
            @atoms_by_name.delete(atom.name) if name_set.empty?
          end
        end
        
        if atom.is_a?(Link)
          outgoing_handles = atom.outgoing.map(&.handle)
          if outgoing_set = @atoms_by_outgoing[outgoing_handles]?
            outgoing_set.delete(atom)
            @atoms_by_outgoing.delete(outgoing_handles) if outgoing_set.empty?
          end
        end
        
        # Update statistics
        @atom_count -= 1
        if atom.node?
          @node_count -= 1
        else
          @link_count -= 1
        end
        
        emit_event(AtomSpaceEvent::ATOM_REMOVED, atom)
        true
      end
    end
    
    # Check if atom has incoming links
    private def has_incoming_links?(atom : Atom) : Bool
      @atoms_by_outgoing.each do |outgoing_handles, link_set|
        return true if outgoing_handles.includes?(atom.handle)
      end
      false
    end
    
    # Get atom by handle
    def get_atom(handle : Handle) : Atom?
      @mutex.synchronize do
        @atoms_by_handle[handle]?
      end
    end
    
    # Get all atoms of a specific type
    def get_atoms_by_type(type : AtomType) : Array(Atom)
      @mutex.synchronize do
        type_set = @atoms_by_type[type]?
        type_set ? type_set.to_a : [] of Atom
      end
    end
    
    # Get nodes by name
    def get_nodes_by_name(name : String, type : AtomType? = nil) : Array(Atom)
      @mutex.synchronize do
        name_set = @atoms_by_name[name]?
        return [] of Atom unless name_set
        
        if type
          name_set.select { |atom| atom.type == type }.to_a
        else
          name_set.to_a
        end
      end
    end
    
    # Get all atoms
    def get_all_atoms : Array(Atom)
      @mutex.synchronize do
        @atoms_by_handle.values
      end
    end
    
    # Count statistics
    def size : UInt64
      @atom_count
    end
    
    def node_count : UInt64
      @node_count
    end
    
    def link_count : UInt64
      @link_count
    end
    
    # Check if atom exists
    def contains?(atom : Atom) : Bool
      @mutex.synchronize do
        @atoms_by_handle.has_key?(atom.handle)
      end
    end
    
    def contains?(handle : Handle) : Bool
      @mutex.synchronize do
        @atoms_by_handle.has_key?(handle)
      end
    end
    
    # Clear all atoms
    def clear
      @mutex.synchronize do
        @atoms_by_handle.clear
        @atoms_by_type.clear
        @atoms_by_name.clear
        @atoms_by_outgoing.clear
        @atom_count = 0_u64
        @node_count = 0_u64
        @link_count = 0_u64
        
        CogUtil::Logger.info("AtomSpace cleared")
      end
    end
    
    # Convenience methods for creating and adding atoms
    def add_node(type : AtomType, name : String, tv : TruthValue = TruthValue::DEFAULT_TV) : Atom
      node = Node.new(type, name, tv)
      add_atom(node)
    end
    
    def add_link(type : AtomType, outgoing : Array(Atom), tv : TruthValue = TruthValue::DEFAULT_TV) : Atom
      link = Link.new(type, outgoing, tv)
      add_atom(link)
    end
    
    # Specific atom creation methods
    def add_concept_node(name : String, tv : TruthValue = TruthValue::DEFAULT_TV) : Atom
      add_node(AtomType::CONCEPT_NODE, name, tv)
    end
    
    def add_predicate_node(name : String, tv : TruthValue = TruthValue::DEFAULT_TV) : Atom
      add_node(AtomType::PREDICATE_NODE, name, tv)
    end
    
    def add_inheritance_link(child : Atom, parent : Atom, tv : TruthValue = TruthValue::DEFAULT_TV) : Atom
      add_link(AtomType::INHERITANCE_LINK, [child, parent], tv)
    end
    
    def add_evaluation_link(predicate : Atom, arguments : Atom, tv : TruthValue = TruthValue::DEFAULT_TV) : Atom
      add_link(AtomType::EVALUATION_LINK, [predicate, arguments], tv)
    end
    
    def add_list_link(atoms : Array(Atom), tv : TruthValue = TruthValue::DEFAULT_TV) : Atom
      add_link(AtomType::LIST_LINK, atoms, tv)
    end
    
    # Event system
    def add_observer(observer : AtomSpaceSignal)
      @observers << observer
    end
    
    def remove_observer(observer : AtomSpaceSignal)
      @observers.delete(observer)
    end
    
    private def emit_event(event : AtomSpaceEvent, atom : Atom)
      @observers.each do |observer|
        begin
          observer.call(event, atom)
        rescue ex
          CogUtil::Logger.error("Error in AtomSpace observer: #{ex}")
        end
      end
    end
    
    # Pattern matching support
    def get_incoming(atom : Atom) : Array(Atom)
      incoming = [] of Atom
      @mutex.synchronize do
        @atoms_by_outgoing.each do |outgoing_handles, link_set|
          if outgoing_handles.includes?(atom.handle)
            incoming.concat(link_set.to_a)
          end
        end
      end
      incoming
    end
    
    # Find atoms matching a pattern
    def find_atoms(pattern : Atom) : Array(Atom)
      results = [] of Atom
      @mutex.synchronize do
        @atoms_by_handle.each_value do |atom|
          results << atom if atom.satisfies?(pattern)
        end
      end
      results
    end
    
    # Statistics and debugging
    def print_statistics
      @mutex.synchronize do
        puts "AtomSpace Statistics:"
        puts "  Total atoms: #{@atom_count}"
        puts "  Nodes: #{@node_count}"
        puts "  Links: #{@link_count}"
        puts "  Types in use: #{@atoms_by_type.size}"
        puts "  Unique names: #{@atoms_by_name.size}"
        puts "  Link patterns: #{@atoms_by_outgoing.size}"
      end
    end
    
    # Export to string representation
    def to_s(io : IO) : Nil
      io << "AtomSpace(#{@atom_count} atoms)"
    end
    
    def inspect(io : IO) : Nil
      @mutex.synchronize do
        io << "AtomSpace {\n"
        @atoms_by_handle.each_value do |atom|
          io << "  "
          atom.inspect(io)
          io << "\n"
        end
        io << "}"
      end
    end
  end
  
  # Global AtomSpace instance management
  module AtomSpaceManager
    @@default_atomspace : AtomSpace?
    
    def self.default_atomspace : AtomSpace
      @@default_atomspace ||= AtomSpace.new
    end
    
    def self.set_default_atomspace(atomspace : AtomSpace)
      @@default_atomspace = atomspace
    end
    
    # Convenience methods using default AtomSpace
    def self.add_atom(atom : Atom) : Atom
      default_atomspace.add_atom(atom)
    end
    
    def self.add_node(type : AtomType, name : String, tv : TruthValue = TruthValue::DEFAULT_TV) : Atom
      default_atomspace.add_node(type, name, tv)
    end
    
    def self.add_link(type : AtomType, outgoing : Array(Atom), tv : TruthValue = TruthValue::DEFAULT_TV) : Atom
      default_atomspace.add_link(type, outgoing, tv)
    end
    
    def self.size : UInt64
      default_atomspace.size
    end
    
    def self.clear
      default_atomspace.clear
    end
  end
  
  # Initialize the AtomSpace module
  def self.initialize
    CogUtil::Logger.info("AtomSpace module initialized")
  end
  
  # Module-level convenience methods
  def self.new_atomspace : AtomSpace
    AtomSpace.new
  end
  
  def self.default_atomspace : AtomSpace
    AtomSpaceManager.default_atomspace
  end
end