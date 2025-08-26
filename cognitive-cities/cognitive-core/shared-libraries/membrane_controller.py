try:
    from opencog.openpsi import *
    from opencog.atomspace import types
    from opencog.type_constructors import *
    OPENPSI_AVAILABLE = True
except ImportError:
    OPENPSI_AVAILABLE = False
    def psi_rule(context, action, goal, stv=None): return f"psi_rule({context}, {action}, {goal})"
    def psi_add_category(category, rule): return f"psi_add_category({category}, {rule})"
    def psi_imply(rule): return f"psi_imply({rule})"
    class TruthValue:
        def __init__(self, mean, confidence): 
            self.mean = mean
            self.confidence = confidence
    def AndLink(*args): return f"AndLink({args})"
    def EvaluationLink(pred, args): return f"EvaluationLink({pred}, {args})"
    def PredicateNode(name): return f"PredicateNode({name})"
    def ConceptNode(name): return f"ConceptNode({name})"
    def ExecutionOutputLink(schema, args): return f"ExecutionOutputLink({schema}, {args})"
    def GroundedSchemaNode(name): return f"GroundedSchemaNode({name})"
    def ListLink(*args): return f"ListLink({args})"

import logging

class MembraneController:
    def __init__(self, atomspace):
        self.atomspace = atomspace
        self.logger = logging.getLogger(__name__)
        self.membrane_categories = {
            "cognitive_membrane": "cerebral_triad",
            "extension_membrane": "somatic_triad", 
            "security_membrane": "autonomic_triad"
        }
        
    def create_membrane_rule(self, context, action, goal, triad):
        """Create P-System membrane rules as OpenPsi ImplicationLinks"""
        try:
            rule = psi_rule(
                context=context,
                action=action, 
                goal=goal,
                stv=TruthValue(0.8, 0.9)
            )
            
            membrane_type = self.get_membrane_type(triad)
            psi_add_category(membrane_type, rule)
            
            self.logger.info(f"Created membrane rule for {triad} in {membrane_type}")
            return rule
            
        except Exception as e:
            self.logger.error(f"Failed to create membrane rule: {e}")
            return None
    
    def get_membrane_type(self, triad):
        """Map triad to P-System membrane type"""
        triad_to_membrane = {
            "cerebral_triad": "cognitive_membrane",
            "somatic_triad": "extension_membrane",
            "autonomic_triad": "security_membrane"
        }
        return triad_to_membrane.get(triad, "unknown_membrane")
    
    def enforce_membrane_boundary(self, source_triad, target_triad, message):
        """Enforce membrane boundaries for inter-triad communication"""
        source_membrane = self.get_membrane_type(source_triad)
        target_membrane = self.get_membrane_type(target_triad)
        
        boundary_rule = self.create_boundary_rule(source_membrane, target_membrane)
        
        if self.validate_boundary_crossing(boundary_rule, message):
            return True
        else:
            self.logger.warning(f"Membrane boundary violation: {source_triad} -> {target_triad}")
            return False
    
    def create_boundary_rule(self, source_membrane, target_membrane):
        """Create boundary enforcement rules between membranes"""
        context = AndLink(
            EvaluationLink(
                PredicateNode("source_membrane"),
                ConceptNode(source_membrane)
            ),
            EvaluationLink(
                PredicateNode("target_membrane"),
                ConceptNode(target_membrane)
            )
        )
        
        action = ExecutionOutputLink(
            GroundedSchemaNode("scm: validate-membrane-crossing"),
            ListLink(
                ConceptNode(source_membrane),
                ConceptNode(target_membrane)
            )
        )
        
        goal = ConceptNode("membrane_security")
        
        return self.create_membrane_rule(context, action, goal, "security")
    
    def validate_boundary_crossing(self, boundary_rule, message):
        """Validate if message can cross membrane boundary"""
        try:
            result = psi_imply(boundary_rule)
            if OPENPSI_AVAILABLE and hasattr(result, 'tv'):
                return result is not None and result.tv.mean > 0.5
            else:
                return True
        except Exception as e:
            self.logger.error(f"Boundary validation failed: {e}")
            return False
    
    def create_hemisphere_rule(self, hemisphere, function_type, context, action):
        """Create rules for hemisphere-specific processing"""
        hemisphere_context = AndLink(
            context,
            EvaluationLink(
                PredicateNode("hemisphere"),
                ConceptNode(hemisphere)
            )
        )
        
        if hemisphere == "right":
            goal = ConceptNode("intuitive_idea_potential")
        else:
            goal = ConceptNode("applied_technique_commitment")
        
        return self.create_membrane_rule(hemisphere_context, action, goal, "cerebral_triad")
    
    def create_polarity_rule(self, polarity, function_type, context, action):
        """Create rules for polarity-specific processing in autonomic triad"""
        polarity_context = AndLink(
            context,
            EvaluationLink(
                PredicateNode("polarity"),
                ConceptNode(polarity)
            )
        )
        
        if polarity == "sympathetic":
            goal = ConceptNode("emotive_technique")
        else:
            goal = ConceptNode("intuitive_feeling_potential")
        
        return self.create_membrane_rule(polarity_context, action, goal, "autonomic_triad")
