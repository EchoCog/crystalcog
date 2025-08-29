# Crystal implementation of PLN (Probabilistic Logic Networks)
# Converted from pln/opencog/pln/

require "../atomspace/atomspace_main"
require "../cogutil/cogutil"

module PLN
  VERSION = "0.1.0"
  
  # PLN Rule interface
  abstract class PLNRule
    abstract def name : String
    abstract def applies_to?(premise : AtomSpace::Atom) : Bool
    abstract def apply(premise : AtomSpace::Atom, atomspace : AtomSpace::AtomSpace) : AtomSpace::Atom?
  end
  
  # Deduction Rule: If A->B and B->C then A->C
  class DeductionRule < PLNRule
    def name : String
      "DeductionRule"
    end
    
    def applies_to?(premise : AtomSpace::Atom) : Bool
      # Check if we have two inheritance links that can be chained
      premise.is_a?(AtomSpace::Link) &&
        premise.type == AtomSpace::AtomType::INHERITANCE_LINK
    end
    
    def apply(premise : AtomSpace::Atom, atomspace : AtomSpace::AtomSpace) : AtomSpace::Atom?
      return nil unless premise.is_a?(AtomSpace::Link)
      return nil unless premise.outgoing.size == 2
      
      # Look for another inheritance link with matching middle term
      a, b = premise.outgoing[0], premise.outgoing[1]
      
      # Find B->C links where B matches our B
      inheritance_links = atomspace.get_atoms_by_type(AtomSpace::AtomType::INHERITANCE_LINK)
      
      inheritance_links.each do |link|
        next unless link.is_a?(AtomSpace::Link)
        next unless link.outgoing.size == 2
        
        b2, c = link.outgoing[0], link.outgoing[1]
        
        if b == b2  # Found matching chain A->B, B->C
          # Calculate new truth value using PLN formula
          tv_ab = premise.truth_value
          tv_bc = link.truth_value
          
          # PLN deduction formula (simplified)
          new_strength = tv_ab.strength * tv_bc.strength
          new_confidence = tv_ab.confidence * tv_bc.confidence * 0.9  # Discount factor
          
          new_tv = AtomSpace::SimpleTruthValue.new(new_strength, new_confidence)
          
          # Create A->C link
          return atomspace.add_link(
            AtomSpace::AtomType::INHERITANCE_LINK,
            [a, c],
            new_tv
          )
        end
      end
      
      nil
    end
  end
  
  # Inversion Rule: If A->B then B->A (with inverted strength)
  class InversionRule < PLNRule
    def name : String
      "InversionRule"
    end
    
    def applies_to?(premise : AtomSpace::Atom) : Bool
      premise.is_a?(AtomSpace::Link) &&
        premise.type == AtomSpace::AtomType::INHERITANCE_LINK
    end
    
    def apply(premise : AtomSpace::Atom, atomspace : AtomSpace::AtomSpace) : AtomSpace::Atom?
      return nil unless premise.is_a?(AtomSpace::Link)
      return nil unless premise.outgoing.size == 2
      
      a, b = premise.outgoing[0], premise.outgoing[1]
      
      # Create inverted truth value
      tv = premise.truth_value
      inverted_strength = 1.0 / (1.0 + (1.0 - tv.strength) / tv.strength)
      inverted_confidence = tv.confidence * 0.8  # Discount for inversion
      
      inverted_tv = AtomSpace::SimpleTruthValue.new(inverted_strength, inverted_confidence)
      
      # Create B->A link
      atomspace.add_link(
        AtomSpace::AtomType::INHERITANCE_LINK,
        [b, a],
        inverted_tv
      )
    end
  end
  
  # PLN Reasoning Engine
  class PLNEngine
    @rules : Array(PLNRule)
    
    def initialize(@atomspace : AtomSpace::AtomSpace)
      @rules = [
        DeductionRule.new,
        InversionRule.new
      ] of PLNRule
    end
    
    def add_rule(rule : PLNRule)
      @rules << rule
    end
    
    def reason(max_iterations : Int32 = 10) : Array(AtomSpace::Atom)
      new_atoms = [] of AtomSpace::Atom
      iterations = 0
      
      while iterations < max_iterations
        iteration_new_atoms = [] of AtomSpace::Atom
        
        # Apply rules to all atoms
        @atomspace.get_all_atoms.each do |atom|
          @rules.each do |rule|
            if rule.applies_to?(atom)
              new_atom = rule.apply(atom, @atomspace)
              if new_atom && !@atomspace.contains?(new_atom)
                iteration_new_atoms << new_atom
                CogUtil::Logger.debug("PLN: Applied #{rule.name} to #{atom}, created #{new_atom}")
              end
            end
          end
        end
        
        # If no new atoms were created, we're done
        break if iteration_new_atoms.empty?
        
        new_atoms.concat(iteration_new_atoms)
        iterations += 1
      end
      
      CogUtil::Logger.info("PLN: Generated #{new_atoms.size} new atoms in #{iterations} iterations")
      new_atoms
    end
    
    # Forward chaining inference
    def forward_chain(target_type : AtomSpace::AtomType, max_steps : Int32 = 5) : Array(AtomSpace::Atom)
      results = [] of AtomSpace::Atom
      
      max_steps.times do
        step_results = reason(1)
        break if step_results.empty?
        
        step_results.each do |atom|
          results << atom if atom.type == target_type
        end
      end
      
      results
    end
    
    # Backward chaining inference
    def backward_chain(goal : AtomSpace::Atom) : Bool
      # Check if goal already exists
      return true if @atomspace.contains?(goal)
      
      # Try to derive the goal using available rules
      @rules.each do |rule|
        # This is a simplified backward chaining
        # In practice, this would be more sophisticated
        if rule.applies_to?(goal)
          premises = find_premises_for_goal(goal, rule)
          premises.each do |premise|
            if @atomspace.contains?(premise)
              derived = rule.apply(premise, @atomspace)
              return true if derived && derived == goal
            end
          end
        end
      end
      
      false
    end
    
    private def find_premises_for_goal(goal : AtomSpace::Atom, rule : PLNRule) : Array(AtomSpace::Atom)
      # This would contain rule-specific logic to find premises
      # that could lead to the goal
      [] of AtomSpace::Atom
    end
  end
  
  # Initialize PLN module
  def self.initialize
    CogUtil::Logger.info("PLN #{VERSION} initialized")
  end
  
  # Convenience method to create PLN engine
  def self.create_engine(atomspace : AtomSpace::AtomSpace) : PLNEngine
    PLNEngine.new(atomspace)
  end
end