# Crystal implementation of OpenCog Pattern Matching Engine
# Converted from opencog/query and related components
#
# This provides pattern matching capabilities for the AtomSpace,
# allowing complex graph queries and variable binding operations.

require "../cogutil/cogutil"
require "../atomspace/atomspace_main"

module PatternMatching
  VERSION = "0.1.0"
  
  # Variable binding represents a mapping from variables to atoms
  alias VariableBinding = Hash(AtomSpace::Atom, AtomSpace::Atom)
  
  # Pattern match result contains the bindings and any matched atoms
  struct MatchResult
    getter bindings : VariableBinding
    getter matched_atoms : Array(AtomSpace::Atom)
    
    def initialize(@bindings : VariableBinding, @matched_atoms : Array(AtomSpace::Atom))
    end
    
    def success?
      !@bindings.empty? || !@matched_atoms.empty?
    end
    
    def empty?
      @bindings.empty? && @matched_atoms.empty?
    end
    
    def to_s(io)
      io << "MatchResult("
      io << "bindings: #{@bindings.size}, "
      io << "matched: #{@matched_atoms.size}"
      io << ")"
    end
  end
  
  # Pattern specification for matching operations
  class Pattern
    getter template : AtomSpace::Atom
    getter variables : Set(AtomSpace::Atom)
    getter constraints : Array(Constraint)
    
    def initialize(@template : AtomSpace::Atom)
      @variables = Set(AtomSpace::Atom).new
      @constraints = Array(Constraint).new
      collect_variables(@template)
    end
    
    def add_constraint(constraint : Constraint)
      @constraints << constraint
    end
    
    # Check if an atom is a variable (VariableNode starting with $)
    def self.variable?(atom : AtomSpace::Atom) : Bool
      atom.type == AtomSpace::AtomType::VARIABLE_NODE &&
      atom.responds_to?(:name) &&
      atom.name.starts_with?("$")
    end
    
    private def collect_variables(atom : AtomSpace::Atom)
      if Pattern.variable?(atom)
        @variables << atom
      elsif atom.responds_to?(:outgoing)
        atom.outgoing.each { |child| collect_variables(child) }
      end
    end
    
    def to_s(io)
      io << "Pattern(template: #{@template}, variables: #{@variables.size})"
    end
  end
  
  # Base class for pattern matching constraints
  abstract class Constraint
    abstract def satisfied?(bindings : VariableBinding, atomspace : AtomSpace::AtomSpace) : Bool
    abstract def to_s(io)
  end
  
  # Type constraint - ensures a variable binds to atoms of specific types
  class TypeConstraint < Constraint
    getter variable : AtomSpace::Atom
    getter allowed_types : Set(AtomSpace::AtomType)
    
    def initialize(@variable : AtomSpace::Atom, @allowed_types : Set(AtomSpace::AtomType))
    end
    
    def initialize(@variable : AtomSpace::Atom, allowed_type : AtomSpace::AtomType)
      @allowed_types = Set{allowed_type}
    end
    
    def satisfied?(bindings : VariableBinding, atomspace : AtomSpace::AtomSpace) : Bool
      bound_atom = bindings[@variable]?
      return true unless bound_atom # No binding yet, constraint doesn't apply
      
      @allowed_types.includes?(bound_atom.type)
    end
    
    def to_s(io)
      io << "TypeConstraint(#{@variable} : #{@allowed_types})"
    end
  end
  
  # Present constraint - ensures atoms exist in the atomspace
  class PresentConstraint < Constraint
    getter atoms : Array(AtomSpace::Atom)
    
    def initialize(@atoms : Array(AtomSpace::Atom))
    end
    
    def initialize(atom : AtomSpace::Atom)
      @atoms = [atom]
    end
    
    def satisfied?(bindings : VariableBinding, atomspace : AtomSpace::AtomSpace) : Bool
      @atoms.all? do |atom|
        # Substitute variables with bindings
        substituted = substitute_variables(atom, bindings)
        atomspace.contains?(substituted)
      end
    end
    
    private def substitute_variables(atom : AtomSpace::Atom, bindings : VariableBinding) : AtomSpace::Atom
      if Pattern.variable?(atom)
        bindings[atom]? || atom
      elsif atom.responds_to?(:outgoing)
        # For links, substitute variables in outgoing atoms
        new_outgoing = atom.outgoing.map { |child| substitute_variables(child, bindings) }
        # Create a new link with substituted outgoing atoms
        # This is simplified - would need proper link creation based on type
        atom # Return original for now
      else
        atom
      end
    end
    
    def to_s(io)
      io << "PresentConstraint(#{@atoms.size} atoms)"
    end
  end
  
  # Main pattern matching engine
  class PatternMatcher
    getter atomspace : AtomSpace::AtomSpace
    
    def initialize(@atomspace : AtomSpace::AtomSpace)
    end
    
    # Find all matches for a given pattern
    def match(pattern : Pattern) : Array(MatchResult)
      results = Array(MatchResult).new
      
      # Start with empty bindings and try to match
      initial_bindings = VariableBinding.new
      find_matches(pattern, pattern.template, initial_bindings, results)
      
      results
    end
    
    # Match a single pattern template and return the first result
    def match_one(template : AtomSpace::Atom) : MatchResult?
      pattern = Pattern.new(template)
      results = match(pattern)
      results.first?
    end
    
    # Find all atoms that could bind to a variable given constraints
    def find_variable_candidates(variable : AtomSpace::Atom, pattern : Pattern) : Array(AtomSpace::Atom)
      # Get type constraints for this variable
      type_constraints = pattern.constraints.select(&.is_a?(TypeConstraint))
                                           .map(&.as(TypeConstraint))
                                           .select(&.variable.==(variable))
      
      if type_constraints.empty?
        # No type constraints, get all concrete atoms
        # We need to get atoms of specific concrete types
        candidates = Array(AtomSpace::Atom).new
        
        # Add all concept nodes
        candidates.concat(@atomspace.get_atoms_by_type(AtomSpace::AtomType::CONCEPT_NODE))
        candidates.concat(@atomspace.get_atoms_by_type(AtomSpace::AtomType::PREDICATE_NODE))
        candidates.concat(@atomspace.get_atoms_by_type(AtomSpace::AtomType::VARIABLE_NODE))
        candidates.concat(@atomspace.get_atoms_by_type(AtomSpace::AtomType::NUMBER_NODE))
        
        # Add all concrete link types
        candidates.concat(@atomspace.get_atoms_by_type(AtomSpace::AtomType::INHERITANCE_LINK))
        candidates.concat(@atomspace.get_atoms_by_type(AtomSpace::AtomType::EVALUATION_LINK))
        candidates.concat(@atomspace.get_atoms_by_type(AtomSpace::AtomType::LIST_LINK))
        candidates.concat(@atomspace.get_atoms_by_type(AtomSpace::AtomType::AND_LINK))
        candidates.concat(@atomspace.get_atoms_by_type(AtomSpace::AtomType::OR_LINK))
        candidates.concat(@atomspace.get_atoms_by_type(AtomSpace::AtomType::NOT_LINK))
        
        candidates.uniq
      else
        # Get atoms matching the type constraints
        candidates = Array(AtomSpace::Atom).new
        type_constraints.each do |constraint|
          constraint.allowed_types.each do |atom_type|
            candidates.concat(@atomspace.get_atoms_by_type(atom_type))
          end
        end
        candidates.uniq
      end
    end
    
    private def find_matches(pattern : Pattern, template : AtomSpace::Atom, 
                           bindings : VariableBinding, results : Array(MatchResult))
      if Pattern.variable?(template)
        # This is a variable, try to bind it
        bind_variable(pattern, template, bindings, results)
      elsif template.responds_to?(:outgoing)
        # This is a link, match the structure
        match_link_structure(pattern, template, bindings, results)
      else
        # This is a concrete atom, check if it exists
        if @atomspace.contains?(template)
          # Check all constraints
          if constraints_satisfied?(pattern, bindings)
            results << MatchResult.new(bindings.dup, [template])
          end
        end
      end
    end
    
    private def bind_variable(pattern : Pattern, variable : AtomSpace::Atom,
                            bindings : VariableBinding, results : Array(MatchResult))
      # If already bound, check consistency
      if existing_binding = bindings[variable]?
        if constraints_satisfied?(pattern, bindings)
          results << MatchResult.new(bindings.dup, [existing_binding])
        end
        return
      end
      
      # Find candidates for this variable
      candidates = find_variable_candidates(variable, pattern)
      
      candidates.each do |candidate|
        # Try binding this variable to the candidate
        new_bindings = bindings.dup
        new_bindings[variable] = candidate
        
        # Check if constraints are satisfied
        if constraints_satisfied?(pattern, new_bindings)
          results << MatchResult.new(new_bindings, [candidate])
        end
      end
    end
    
    private def match_link_structure(pattern : Pattern, template : AtomSpace::Atom,
                                   bindings : VariableBinding, results : Array(MatchResult))
      # Find atoms in atomspace with the same type as template
      candidates = @atomspace.get_atoms_by_type(template.type)
      
      candidates.each do |candidate|
        next unless candidate.responds_to?(:outgoing)
        next unless candidate.outgoing.size == template.outgoing.size
        
        # Try to match each outgoing atom
        if match_outgoing_atoms(pattern, template.outgoing, candidate.outgoing, bindings, results)
          # Successfully matched this candidate
          if constraints_satisfied?(pattern, bindings)
            results << MatchResult.new(bindings.dup, [candidate])
          end
        end
      end
    end
    
    private def match_outgoing_atoms(pattern : Pattern, template_outgoing : Array(AtomSpace::Atom),
                                   candidate_outgoing : Array(AtomSpace::Atom), 
                                   bindings : VariableBinding, results : Array(MatchResult)) : Bool
      # Simple implementation: match each position independently
      # More sophisticated implementations would handle permutations and complex matching
      
      template_outgoing.zip(candidate_outgoing) do |template_atom, candidate_atom|
        if Pattern.variable?(template_atom)
          # Check if this variable can bind to the candidate
          if existing_binding = bindings[template_atom]?
            return false unless existing_binding == candidate_atom
          else
            bindings[template_atom] = candidate_atom
          end
        else
          # Must match exactly
          return false unless template_atom == candidate_atom
        end
      end
      
      true
    end
    
    private def constraints_satisfied?(pattern : Pattern, bindings : VariableBinding) : Bool
      pattern.constraints.all? { |constraint| constraint.satisfied?(bindings, @atomspace) }
    end
  end
  
  # Initialize the PatternMatching subsystem
  def self.initialize
    CogUtil::Logger.info("PatternMatching #{VERSION} initializing")
    
    # Initialize dependencies
    CogUtil.initialize unless @@cogutil_initialized
    AtomSpace.initialize unless @@atomspace_initialized
    
    CogUtil::Logger.info("PatternMatching #{VERSION} initialized")
  end
  
  # Track initialization state
  @@cogutil_initialized = false
  @@atomspace_initialized = false
  
  # Exception classes for PatternMatching
  class PatternMatchingException < CogUtil::OpenCogException
  end
  
  class PatternCompilationException < PatternMatchingException
  end
  
  class MatchingTimeoutException < PatternMatchingException
  end
end