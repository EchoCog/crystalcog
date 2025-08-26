from cognitive_core.shared_libraries.atomspace_manager import CognitiveAtomSpaceManager
from cognitive_core.shared_libraries.membrane_controller import MembraneController
from opencog.ghost import *
from opencog.type_constructors import *
import logging
import asyncio

class ThoughtService:
    def __init__(self):
        self.atomspace_manager = CognitiveAtomSpaceManager()
        self.membrane_controller = MembraneController(self.atomspace_manager.atomspace)
        self.hemisphere = "right"
        self.component_number = 7
        self.component_letter = "T"
        self.logger = logging.getLogger(__name__)
        
        self.setup_system5_mapping()
        self.setup_hemisphere_rules()
        
    def setup_system5_mapping(self):
        """Map this service to System-5 CNS component T(7)"""
        self.atomspace_manager.store_system5_mapping(
            self.component_number,
            self.component_letter,
            "thought_service",
            "cerebral_triad"
        )
        
    def setup_hemisphere_rules(self):
        """Setup right hemisphere intuitive processing rules"""
        context = EvaluationLink(
            PredicateNode("requires_intuitive_processing"),
            VariableNode("$input")
        )
        
        action = ExecutionOutputLink(
            GroundedSchemaNode("py: generate_intuitive_ideas"),
            VariableNode("$input")
        )
        
        self.intuitive_rule = self.membrane_controller.create_hemisphere_rule(
            "right", "intuitive_processing", context, action
        )
        
    async def generate_intuitive_ideas(self, sensory_input):
        """Generate creative, intuitive responses using right hemisphere processing"""
        try:
            ghost_rules = f"""
            goal: (intuitive_processing=0.8)
            r: (generate ideas about _*) ^generate_creative_response('_0)
            r: (brainstorm _*) ^intuitive_brainstorm('_0)
            r: (imagine _*) ^creative_imagination('_0)
            """
            
            ideas = await self.process_with_ghost(ghost_rules, sensory_input)
            
            self.logger.info(f"Generated {len(ideas)} intuitive ideas")
            return ideas
            
        except Exception as e:
            self.logger.error(f"Intuitive idea generation failed: {e}")
            return []
    
    async def process_with_ghost(self, ghost_rules, input_data):
        """Process input using Ghost DSL rules"""
        try:
            ghost_parse_string(ghost_rules)
            
            response = ghost(str(input_data))
            
            return self.extract_ideas_from_response(response)
            
        except Exception as e:
            self.logger.error(f"Ghost processing failed: {e}")
            return []
    
    def extract_ideas_from_response(self, response):
        """Extract structured ideas from Ghost response"""
        if not response:
            return []
            
        ideas = []
        
        for atom in response:
            if atom.type == types.ConceptNode:
                ideas.append({
                    "concept": atom.name,
                    "hemisphere": self.hemisphere,
                    "processing_type": "intuitive",
                    "confidence": 0.8
                })
        
        return ideas
    
    def create_idea_potential(self, concept, context):
        """Create idea potential atoms for right hemisphere processing"""
        idea_node = ConceptNode(f"idea_{concept}")
        potential_node = ConceptNode("intuitive_idea_potential")
        context_node = ConceptNode(context)
        
        return EvaluationLink(
            potential_node,
            ListLink(idea_node, context_node)
        )
    
    async def communicate_with_processing_director(self, ideas):
        """Send generated ideas to processing director for coordination"""
        message = {
            "source": "thought_service",
            "target": "processing_director",
            "type": "idea_generation",
            "data": ideas,
            "hemisphere": self.hemisphere
        }
        
        if self.membrane_controller.enforce_membrane_boundary(
            "cerebral_triad", "cerebral_triad", message
        ):
            return await self.send_to_processing_director(message)
        else:
            self.logger.warning("Membrane boundary prevented communication")
            return False
    
    async def send_to_processing_director(self, message):
        """Send message to processing director service"""
        try:
            return True
        except Exception as e:
            self.logger.error(f"Communication with processing director failed: {e}")
            return False
