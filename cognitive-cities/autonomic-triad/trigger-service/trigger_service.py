from cognitive_core.shared_libraries.atomspace_manager import CognitiveAtomSpaceManager
from cognitive_core.shared_libraries.membrane_controller import MembraneController
from opencog.type_constructors import *
import logging
import asyncio
from typing import Dict, List, Callable

class TriggerService:
    def __init__(self):
        self.atomspace_manager = CognitiveAtomSpaceManager()
        self.membrane_controller = MembraneController(self.atomspace_manager.atomspace)
        self.component_number = 7
        self.component_letter = "T"
        self.polarity = "parasympathetic"
        self.trigger_handlers = {}
        self.logger = logging.getLogger(__name__)
        
        self.setup_system5_mapping()
        self.setup_parasympathetic_rules()
        self.register_default_triggers()
        
    def setup_system5_mapping(self):
        """Map this service to System-5 CNS component T(7) in autonomic triad"""
        self.atomspace_manager.store_system5_mapping(
            self.component_number,
            self.component_letter,
            "trigger_service",
            "autonomic_triad"
        )
        
    def setup_parasympathetic_rules(self):
        """Setup parasympathetic nervous system response rules"""
        context = AndLink(
            EvaluationLink(
                PredicateNode("has_trigger_event"),
                VariableNode("$event")
            ),
            EvaluationLink(
                PredicateNode("polarity"),
                ConceptNode("parasympathetic")
            )
        )
        
        action = ExecutionOutputLink(
            GroundedSchemaNode("py: parasympathetic_response"),
            VariableNode("$event")
        )
        
        self.parasympathetic_rule = self.membrane_controller.create_polarity_rule(
            "parasympathetic", "trigger_response", context, action
        )
        
    def register_default_triggers(self):
        """Register default trigger handlers"""
        self.register_trigger("stress_detected", self.handle_stress_trigger)
        self.register_trigger("low_energy", self.handle_energy_trigger)
        self.register_trigger("emotional_overload", self.handle_emotional_trigger)
        self.register_trigger("system_idle", self.handle_idle_trigger)
        self.register_trigger("user_interaction", self.handle_interaction_trigger)
        
    def register_trigger(self, trigger_type: str, handler: Callable):
        """Register a trigger handler"""
        self.trigger_handlers[trigger_type] = handler
        self.logger.info(f"Registered trigger handler: {trigger_type}")
        
    async def handle_automatic_triggers(self, event: Dict):
        """Handle automatic responses with parasympathetic processing"""
        try:
            event_type = event.get('type', 'unknown')
            
            if event_type in self.trigger_handlers:
                response = await self.parasympathetic_response(event)
                await self.trigger_handlers[event_type](event, response)
            else:
                self.logger.warning(f"No handler for trigger type: {event_type}")
                response = await self.default_parasympathetic_response(event)
            
            self.logger.info(f"Handled trigger: {event_type}")
            return response
            
        except Exception as e:
            self.logger.error(f"Trigger handling failed: {e}")
            return {"status": "failed", "error": str(e)}
    
    async def parasympathetic_response(self, event: Dict):
        """Implement parasympathetic nervous system response"""
        response = {
            "polarity": self.polarity,
            "response_type": "calming",
            "actions": [],
            "emotional_state": "balanced",
            "intuitive_feeling": await self.generate_intuitive_feeling(event),
            "timestamp": event.get('timestamp')
        }
        
        event_type = event.get('type', 'unknown')
        
        if event_type == "stress_detected":
            response["actions"] = ["reduce_arousal", "increase_calm", "slow_breathing"]
        elif event_type == "emotional_overload":
            response["actions"] = ["emotional_regulation", "mindfulness", "grounding"]
        elif event_type == "system_idle":
            response["actions"] = ["maintenance_mode", "energy_conservation", "reflection"]
        
        await self.store_response_atoms(response)
        return response
    
    async def generate_intuitive_feeling(self, event: Dict) -> Dict:
        """Generate intuitive feeling response (parasympathetic characteristic)"""
        feeling = {
            "type": "intuitive_feeling_potential",
            "intensity": 0.6,
            "quality": "peaceful",
            "direction": "inward"
        }
        
        event_type = event.get('type', 'unknown')
        
        if event_type == "stress_detected":
            feeling.update({
                "quality": "soothing",
                "intensity": 0.8,
                "message": "system_needs_rest"
            })
        elif event_type == "user_interaction":
            feeling.update({
                "quality": "welcoming",
                "intensity": 0.7,
                "message": "positive_engagement"
            })
        
        return feeling
    
    async def handle_stress_trigger(self, event: Dict, response: Dict):
        """Handle stress detection trigger"""
        stress_level = event.get('data', {}).get('stress_level', 0.5)
        
        if stress_level > 0.7:
            await self.initiate_deep_relaxation()
        else:
            await self.initiate_mild_calming()
        
        await self.notify_triads("stress_response_initiated", response)
        
    async def handle_energy_trigger(self, event: Dict, response: Dict):
        """Handle low energy trigger"""
        energy_level = event.get('data', {}).get('energy_level', 0.5)
        
        if energy_level < 0.3:
            await self.initiate_energy_conservation()
        else:
            await self.initiate_gentle_restoration()
        
        await self.notify_triads("energy_response_initiated", response)
        
    async def handle_emotional_trigger(self, event: Dict, response: Dict):
        """Handle emotional overload trigger"""
        emotion_intensity = event.get('data', {}).get('emotion_intensity', 0.5)
        
        if emotion_intensity > 0.8:
            await self.initiate_emotional_regulation()
        else:
            await self.initiate_emotional_balance()
        
        await self.notify_triads("emotional_response_initiated", response)
        
    async def handle_idle_trigger(self, event: Dict, response: Dict):
        """Handle system idle trigger"""
        idle_duration = event.get('data', {}).get('idle_duration', 0)
        
        if idle_duration > 300:
            await self.initiate_maintenance_mode()
        else:
            await self.initiate_background_processing()
        
        await self.notify_triads("idle_response_initiated", response)
        
    async def handle_interaction_trigger(self, event: Dict, response: Dict):
        """Handle user interaction trigger"""
        interaction_type = event.get('data', {}).get('interaction_type', 'unknown')
        
        if interaction_type == "positive":
            await self.enhance_positive_state()
        elif interaction_type == "negative":
            await self.initiate_conflict_resolution()
        else:
            await self.maintain_neutral_state()
        
        await self.notify_triads("interaction_response_initiated", response)
        
    async def initiate_deep_relaxation(self):
        """Initiate deep relaxation response"""
        relaxation_actions = [
            "slow_system_processes",
            "reduce_sensory_sensitivity", 
            "activate_calming_patterns"
        ]
        
        for action in relaxation_actions:
            await self.execute_parasympathetic_action(action)
        
        self.logger.info("Deep relaxation response initiated")
        
    async def initiate_mild_calming(self):
        """Initiate mild calming response"""
        calming_actions = [
            "gentle_process_slowdown",
            "increase_attention_focus"
        ]
        
        for action in calming_actions:
            await self.execute_parasympathetic_action(action)
        
        self.logger.info("Mild calming response initiated")
        
    async def initiate_energy_conservation(self):
        """Initiate energy conservation mode"""
        conservation_actions = [
            "reduce_non_essential_processes",
            "lower_processing_frequency",
            "minimize_output_generation"
        ]
        
        for action in conservation_actions:
            await self.execute_parasympathetic_action(action)
        
        self.logger.info("Energy conservation mode initiated")
        
    async def initiate_emotional_regulation(self):
        """Initiate emotional regulation response"""
        regulation_actions = [
            "balance_emotional_processing",
            "increase_rational_oversight",
            "moderate_response_intensity"
        ]
        
        for action in regulation_actions:
            await self.execute_parasympathetic_action(action)
        
        self.logger.info("Emotional regulation response initiated")
        
    async def execute_parasympathetic_action(self, action: str):
        """Execute a specific parasympathetic action"""
        action_node = ConceptNode(action)
        polarity_node = ConceptNode(self.polarity)
        
        execution_link = EvaluationLink(
            PredicateNode("executes_action"),
            ListLink(polarity_node, action_node)
        )
        
        self.atomspace_manager.atomspace.add_link(execution_link.type, execution_link.out)
        self.logger.info(f"Executed parasympathetic action: {action}")
        
    async def default_parasympathetic_response(self, event: Dict):
        """Default parasympathetic response for unknown events"""
        response = {
            "polarity": self.polarity,
            "response_type": "default_calming",
            "actions": ["maintain_balance", "gentle_monitoring"],
            "emotional_state": "neutral_calm"
        }
        
        return response
        
    async def notify_triads(self, event_type: str, response_data: Dict):
        """Notify other triads of trigger responses"""
        message = {
            "source": "trigger_service",
            "event_type": event_type,
            "response_data": response_data,
            "polarity": self.polarity
        }
        
        await self.send_to_cerebral_triad(message)
        await self.send_to_somatic_triad(message)
        
    async def send_to_cerebral_triad(self, message: Dict):
        """Send trigger response to cerebral triad"""
        if self.membrane_controller.enforce_membrane_boundary(
            "autonomic_triad", "cerebral_triad", message
        ):
            self.logger.info("Trigger response sent to cerebral triad")
            return True
        else:
            self.logger.warning("Membrane boundary prevented cerebral communication")
            return False
            
    async def send_to_somatic_triad(self, message: Dict):
        """Send trigger response to somatic triad"""
        if self.membrane_controller.enforce_membrane_boundary(
            "autonomic_triad", "somatic_triad", message
        ):
            self.logger.info("Trigger response sent to somatic triad")
            return True
        else:
            self.logger.warning("Membrane boundary prevented somatic communication")
            return False
            
    async def store_response_atoms(self, response: Dict):
        """Store trigger response as atoms in AtomSpace"""
        response_node = ConceptNode("trigger_response")
        polarity_node = ConceptNode(response["polarity"])
        
        self.atomspace_manager.atomspace.add_link(
            types.EvaluationLink,
            [
                PredicateNode("has_polarity"),
                ListLink(response_node, polarity_node)
            ]
        )
        
        for action in response.get("actions", []):
            action_node = ConceptNode(action)
            self.atomspace_manager.atomspace.add_link(
                types.EvaluationLink,
                [
                    PredicateNode("includes_action"),
                    ListLink(response_node, action_node)
                ]
            )
