# Crystal implementation of URE (Unified Rule Engine)
# Converted from ure/opencog/ure/

require "../atomspace/atomspace_main"
require "../cogutil/cogutil"

module URE
  VERSION = "0.1.0"
  
  # Generic rule interface for URE
  abstract class Rule
    abstract def name : String
    abstract def premises : Array(AtomSpace::AtomType)
    abstract def conclusion : AtomSpace::AtomType
    abstract def apply(premises : Array(AtomSpace::Atom), atomspace : AtomSpace::AtomSpace) : AtomSpace::Atom?
    abstract def fitness(premises : Array(AtomSpace::Atom)) : Float64
  end
  
  # Rule to handle simple logical conjunction
  class ConjunctionRule < Rule
    def name : String
      "ConjunctionRule"
    end
    
    def premises : Array(AtomSpace::AtomType)
      [AtomSpace::AtomType::EVALUATION_LINK, AtomSpace::AtomType::EVALUATION_LINK]
    end
    
    def conclusion : AtomSpace::AtomType
      AtomSpace::AtomType::AND_LINK
    end
    
    def apply(premises : Array(AtomSpace::Atom), atomspace : AtomSpace::AtomSpace) : AtomSpace::Atom?
      return nil unless premises.size >= 2
      
      # Calculate conjunction truth value
      min_strength = premises.map(&.truth_value.strength).min
      min_confidence = premises.map(&.truth_value.confidence).min
      
      tv = AtomSpace::SimpleTruthValue.new(min_strength, min_confidence * 0.9)
      
      # Create AND link
      atomspace.add_link(AtomSpace::AtomType::AND_LINK, premises, tv)
    end
    
    def fitness(premises : Array(AtomSpace::Atom)) : Float64
      # Fitness based on truth value confidence
      premises.map(&.truth_value.confidence).sum / premises.size
    end
  end
  
  # Rule to handle modus ponens inference
  class ModusPonensRule < Rule
    def name : String
      "ModusPonensRule"
    end
    
    def premises : Array(AtomSpace::AtomType)
      [AtomSpace::AtomType::IMPLICATION_LINK, AtomSpace::AtomType::EVALUATION_LINK]
    end
    
    def conclusion : AtomSpace::AtomType
      AtomSpace::AtomType::EVALUATION_LINK
    end
    
    def apply(premises : Array(AtomSpace::Atom), atomspace : AtomSpace::AtomSpace) : AtomSpace::Atom?
      return nil unless premises.size == 2
      
      implication, antecedent = premises[0], premises[1]
      
      # Check if implication structure matches
      return nil unless implication.is_a?(AtomSpace::Link)
      return nil unless implication.outgoing.size == 2
      
      if_part, then_part = implication.outgoing[0], implication.outgoing[1]
      
      # Check if antecedent matches the if_part
      return nil unless antecedent == if_part
      
      # Calculate conclusion truth value using modus ponens formula
      tv_impl = implication.truth_value
      tv_ante = antecedent.truth_value
      
      # Simplified modus ponens: min(P(A), P(A->B))
      new_strength = [tv_ante.strength, tv_impl.strength].min
      new_confidence = tv_ante.confidence * tv_impl.confidence * 0.95
      
      tv = AtomSpace::SimpleTruthValue.new(new_strength, new_confidence)
      
      # Return the consequent with new truth value
      then_part.truth_value = tv
      then_part
    end
    
    def fitness(premises : Array(AtomSpace::Atom)) : Float64
      premises.map(&.truth_value.strength).min
    end
  end
  
  # Forward chainer for URE
  class ForwardChainer
    @rules : Array(Rule)
    @max_iterations : Int32
    
    def initialize(@atomspace : AtomSpace::AtomSpace, @max_iterations = 100)
      @rules = [] of Rule
    end
    
    def add_rule(rule : Rule)
      @rules << rule
    end
    
    def add_default_rules
      add_rule(ConjunctionRule.new)
      add_rule(ModusPonensRule.new)
    end
    
    def run : Array(AtomSpace::Atom)
      new_atoms = [] of AtomSpace::Atom
      iterations = 0
      
      while iterations < @max_iterations
        step_atoms = step_forward
        break if step_atoms.empty?
        
        new_atoms.concat(step_atoms)
        iterations += 1
        
        CogUtil::Logger.debug("URE: Forward step #{iterations}, generated #{step_atoms.size} atoms")
      end
      
      CogUtil::Logger.info("URE: Forward chaining completed in #{iterations} steps, generated #{new_atoms.size} atoms")
      new_atoms
    end
    
    private def step_forward : Array(AtomSpace::Atom)
      step_atoms = [] of AtomSpace::Atom
      
      @rules.each do |rule|
        rule_applications = find_rule_applications(rule)
        
        rule_applications.each do |premises|
          fitness = rule.fitness(premises)
          next if fitness < 0.1  # Skip low-fitness applications
          
          result = rule.apply(premises, @atomspace)
          if result && !@atomspace.contains?(result)
            step_atoms << result
            CogUtil::Logger.debug("URE: Applied #{rule.name}, fitness: #{fitness}")
          end
        end
      end
      
      step_atoms
    end
    
    private def find_rule_applications(rule : Rule) : Array(Array(AtomSpace::Atom))
      applications = [] of Array(AtomSpace::Atom)
      premise_types = rule.premises
      
      # Simple case: rules with specific premise requirements
      case premise_types.size
      when 1
        atoms = @atomspace.get_atoms_by_type(premise_types[0])
        atoms.each { |atom| applications << [atom] }
      when 2
        atoms1 = @atomspace.get_atoms_by_type(premise_types[0])
        atoms2 = @atomspace.get_atoms_by_type(premise_types[1])
        
        atoms1.each do |a1|
          atoms2.each do |a2|
            applications << [a1, a2] if a1 != a2
          end
        end
      end
      
      applications
    end
  end
  
  # Backward chainer for URE
  class BackwardChainer
    @rules : Array(Rule)
    @max_depth : Int32
    
    def initialize(@atomspace : AtomSpace::AtomSpace, @max_depth = 10)
      @rules = [] of Rule
    end
    
    def add_rule(rule : Rule)
      @rules << rule
    end
    
    def add_default_rules
      add_rule(ConjunctionRule.new)
      add_rule(ModusPonensRule.new)
    end
    
    def query(target : AtomSpace::Atom, depth = 0) : Bool
      return true if @atomspace.contains?(target)
      return false if depth >= @max_depth
      
      @rules.each do |rule|
        if rule.conclusion == target.type
          if search_premises(rule, target, depth + 1)
            return true
          end
        end
      end
      
      false
    end
    
    private def search_premises(rule : Rule, target : AtomSpace::Atom, depth : Int32) : Bool
      # This is a simplified backward search
      # In practice, this would involve more sophisticated unification
      premise_types = rule.premises
      
      premise_types.each do |ptype|
        potential_premises = @atomspace.get_atoms_by_type(ptype)
        
        potential_premises.each do |premise|
          # Try to apply rule with this premise
          result = rule.apply([premise], @atomspace)
          if result && atoms_unify?(result, target)
            return true
          end
        end
      end
      
      false
    end
    
    private def atoms_unify?(atom1 : AtomSpace::Atom, atom2 : AtomSpace::Atom) : Bool
      # Simplified unification - just check for equality
      atom1 == atom2
    end
  end
  
  # Main URE engine combining forward and backward chaining
  class UREEngine
    getter forward_chainer : ForwardChainer
    getter backward_chainer : BackwardChainer
    
    def initialize(@atomspace : AtomSpace::AtomSpace)
      @forward_chainer = ForwardChainer.new(@atomspace)
      @backward_chainer = BackwardChainer.new(@atomspace)
      
      # Add default rules
      @forward_chainer.add_default_rules
      @backward_chainer.add_default_rules
    end
    
    def add_rule(rule : Rule)
      @forward_chainer.add_rule(rule)
      @backward_chainer.add_rule(rule)
    end
    
    def forward_chain(steps : Int32 = 10) : Array(AtomSpace::Atom)
      @forward_chainer.run
    end
    
    def backward_chain(goal : AtomSpace::Atom) : Bool
      @backward_chainer.query(goal)
    end
    
    def mixed_chain(goal : AtomSpace::Atom, forward_steps : Int32 = 5) : Bool
      # Try forward chaining first
      forward_chain(forward_steps)
      
      # Then backward chain to goal
      backward_chain(goal)
    end
  end
  
  # Initialize URE module
  def self.initialize
    CogUtil::Logger.info("URE #{VERSION} initialized")
  end
  
  # Convenience method to create URE engine
  def self.create_engine(atomspace : AtomSpace::AtomSpace) : UREEngine
    UREEngine.new(atomspace)
  end
end