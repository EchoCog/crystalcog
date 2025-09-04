# Pattern Matching main module entry point
# This provides convenient access to pattern matching functionality

require "./pattern_matching"

module PatternMatching
  # Convenience methods for common pattern matching operations
  module Utils
    # Create a simple variable with the given name
    def self.variable(name : String) : AtomSpace::Atom
      AtomSpace::VariableNode.new("$#{name}")
    end
    
    # Create a type constraint for a variable
    def self.type_constraint(variable : AtomSpace::Atom, atom_type : AtomSpace::AtomType) : TypeConstraint
      TypeConstraint.new(variable, atom_type)
    end
    
    # Create a present constraint for atoms
    def self.present_constraint(atoms : Array(AtomSpace::Atom)) : PresentConstraint
      PresentConstraint.new(atoms)
    end
    
    # Create an absent constraint for atoms
    def self.absent_constraint(atoms : Array(AtomSpace::Atom)) : AbsentConstraint
      AbsentConstraint.new(atoms)
    end
    
    # Create an equality constraint between two atoms
    def self.equality_constraint(left : AtomSpace::Atom, right : AtomSpace::Atom) : EqualityConstraint
      EqualityConstraint.new(left, right)
    end
    
    # Create a greater than constraint for numeric comparisons
    def self.greater_than_constraint(left : AtomSpace::Atom, right : AtomSpace::Atom) : GreaterThanConstraint
      GreaterThanConstraint.new(left, right)
    end
    
    # Quick match for simple inheritance patterns
    def self.match_inheritance(atomspace : AtomSpace::AtomSpace, child : String?, parent : String?) : Array(MatchResult)
      matcher = PatternMatcher.new(atomspace)
      
      child_atom = if child
        atomspace.add_concept_node(child)
      else
        variable("child")
      end
      
      parent_atom = if parent
        atomspace.add_concept_node(parent)
      else
        variable("parent")
      end
      
      pattern_atom = AtomSpace::InheritanceLink.new(child_atom, parent_atom)
      pattern = Pattern.new(pattern_atom)
      
      matcher.match(pattern)
    end
    
    # Quick match for simple evaluation patterns
    def self.match_evaluation(atomspace : AtomSpace::AtomSpace, predicate : String?, 
                            subject : String?, object : String?) : Array(MatchResult)
      matcher = PatternMatcher.new(atomspace)
      
      pred_atom = if predicate
        atomspace.add_predicate_node(predicate)
      else
        variable("predicate")
      end
      
      subj_atom = if subject
        atomspace.add_concept_node(subject)
      else
        variable("subject")
      end
      
      obj_atom = if object
        atomspace.add_concept_node(object)
      else
        variable("object")
      end
      
      args = AtomSpace::ListLink.new([subj_atom, obj_atom].map(&.as(AtomSpace::Atom)))
      pattern_atom = AtomSpace::EvaluationLink.new(pred_atom, args)
      pattern = Pattern.new(pattern_atom)
      
      matcher.match(pattern)
    end
  end
  
  # Query builder for more complex patterns
  class QueryBuilder
    getter atomspace : AtomSpace::AtomSpace
    getter variables : Hash(String, AtomSpace::Atom)
    getter constraints : Array(Constraint)
    
    def initialize(@atomspace : AtomSpace::AtomSpace)
      @variables = Hash(String, AtomSpace::Atom).new
      @constraints = Array(Constraint).new
    end
    
    # Add a variable to the query
    def variable(name : String) : AtomSpace::Atom
      unless @variables.has_key?(name)
        @variables[name] = Utils.variable(name)
      end
      @variables[name]
    end
    
    # Add a type constraint
    def constrain_type(var_name : String, atom_type : AtomSpace::AtomType)
      var = variable(var_name)
      @constraints << TypeConstraint.new(var, atom_type)
      self
    end
    
    # Require atoms to be present
    def require_present(atoms : Array(AtomSpace::Atom))
      @constraints << PresentConstraint.new(atoms)
      self
    end
    
    # Require atoms to be absent
    def require_absent(atoms : Array(AtomSpace::Atom))
      @constraints << AbsentConstraint.new(atoms)
      self
    end
    
    # Add equality constraint between variables or atoms
    def constrain_equal(left : String | AtomSpace::Atom, right : String | AtomSpace::Atom)
      left_atom = left.is_a?(String) ? variable(left) : left
      right_atom = right.is_a?(String) ? variable(right) : right
      @constraints << EqualityConstraint.new(left_atom, right_atom)
      self
    end
    
    # Add greater than constraint for numeric comparisons
    def constrain_greater_than(left : String | AtomSpace::Atom, right : String | AtomSpace::Atom)
      left_atom = left.is_a?(String) ? variable(left) : left
      right_atom = right.is_a?(String) ? variable(right) : right
      @constraints << GreaterThanConstraint.new(left_atom, right_atom)
      self
    end
    
    # Build an inheritance pattern
    def inheritance(child : String | AtomSpace::Atom, parent : String | AtomSpace::Atom) : AtomSpace::Atom
      child_atom = child.is_a?(String) ? variable(child) : child
      parent_atom = parent.is_a?(String) ? variable(parent) : parent
      AtomSpace::InheritanceLink.new(child_atom, parent_atom)
    end
    
    # Build an evaluation pattern
    def evaluation(predicate : String | AtomSpace::Atom, subject : String | AtomSpace::Atom, 
                  object : String | AtomSpace::Atom) : AtomSpace::Atom
      pred_atom = predicate.is_a?(String) ? variable(predicate) : predicate
      subj_atom = subject.is_a?(String) ? variable(subject) : subject
      obj_atom = object.is_a?(String) ? variable(object) : object
      
      args = AtomSpace::ListLink.new([subj_atom, obj_atom].map(&.as(AtomSpace::Atom)))
      AtomSpace::EvaluationLink.new(pred_atom, args)
    end
    
    # Execute the query with the given pattern
    def execute(template : AtomSpace::Atom) : Array(MatchResult)
      pattern = Pattern.new(template)
      @constraints.each { |constraint| pattern.add_constraint(constraint) }
      
      matcher = PatternMatcher.new(@atomspace)
      matcher.match(pattern)
    end
  end
end