from cognitive_core.shared_libraries.atomspace_manager import CognitiveAtomSpaceManager
from cognitive_core.shared_libraries.membrane_controller import MembraneController
from opencog.type_constructors import *
import logging
import asyncio
from typing import Dict, List

class MotorControlService:
    def __init__(self):
        self.atomspace_manager = CognitiveAtomSpaceManager()
        self.membrane_controller = MembraneController(self.atomspace_manager.atomspace)
        self.component_number = 1
        self.component_letter = "M"
        self.eva_integration = True
        self.behavior_queue = []
        self.logger = logging.getLogger(__name__)
        
        self.setup_system5_mapping()
        self.setup_motor_control_rules()
        
    def setup_system5_mapping(self):
        """Map this service to System-5 CNS component M(1)"""
        self.atomspace_manager.store_system5_mapping(
            self.component_number,
            self.component_letter,
            "motor_control_service",
            "somatic_triad"
        )
        
    def setup_motor_control_rules(self):
        """Setup motor control rules for behavior execution"""
        context = AndLink(
            EvaluationLink(
                PredicateNode("has_motor_command"),
                VariableNode("$command")
            ),
            EvaluationLink(
                PredicateNode("motor_system_available"),
                ConceptNode("true")
            )
        )
        
        action = ExecutionOutputLink(
            GroundedSchemaNode("py: execute_motor_command"),
            VariableNode("$command")
        )
        
        goal = ConceptNode("somatic_balance_performance")
        
        self.motor_rule = self.membrane_controller.create_membrane_rule(
            context, action, goal, "somatic_triad"
        )
        
    async def execute_motor_command(self, command: Dict):
        """Execute motor commands through Eva animation system"""
        try:
            if self.eva_integration:
                result = await self.eva_motor_execute(command)
            else:
                result = await self.simulate_motor_execution(command)
            
            self.logger.info(f"Motor command executed: {command.get('type', 'unknown')}")
            return result
            
        except Exception as e:
            self.logger.error(f"Motor command execution failed: {e}")
            return {"status": "failed", "error": str(e)}
    
    async def eva_motor_execute(self, command: Dict):
        """Execute motor command through Eva animation system"""
        command_type = command.get('type', 'unknown')
        
        if command_type == 'facial_expression':
            return await self.execute_facial_expression(command)
        elif command_type == 'gesture':
            return await self.execute_gesture(command)
        elif command_type == 'head_movement':
            return await self.execute_head_movement(command)
        elif command_type == 'speech_animation':
            return await self.execute_speech_animation(command)
        else:
            self.logger.warning(f"Unknown motor command type: {command_type}")
            return {"status": "unknown_command"}
    
    async def execute_facial_expression(self, command: Dict):
        """Execute facial expression through Eva system"""
        expression = command.get('expression', 'neutral')
        intensity = command.get('intensity', 0.5)
        duration = command.get('duration', 2.0)
        
        eva_command = {
            "type": "facial_expression",
            "expression": expression,
            "intensity": intensity,
            "duration": duration
        }
        
        self.logger.info(f"Executing facial expression: {expression}")
        return {"status": "executed", "command": eva_command}
    
    async def execute_gesture(self, command: Dict):
        """Execute gesture through Eva system"""
        gesture = command.get('gesture', 'neutral')
        hand = command.get('hand', 'both')
        intensity = command.get('intensity', 0.5)
        
        eva_command = {
            "type": "gesture",
            "gesture": gesture,
            "hand": hand,
            "intensity": intensity
        }
        
        self.logger.info(f"Executing gesture: {gesture}")
        return {"status": "executed", "command": eva_command}
    
    async def execute_head_movement(self, command: Dict):
        """Execute head movement through Eva system"""
        direction = command.get('direction', 'center')
        angle = command.get('angle', 0)
        speed = command.get('speed', 1.0)
        
        eva_command = {
            "type": "head_movement",
            "direction": direction,
            "angle": angle,
            "speed": speed
        }
        
        self.logger.info(f"Executing head movement: {direction}")
        return {"status": "executed", "command": eva_command}
    
    async def execute_speech_animation(self, command: Dict):
        """Execute speech animation through Eva system"""
        text = command.get('text', '')
        emotion = command.get('emotion', 'neutral')
        speed = command.get('speed', 1.0)
        
        eva_command = {
            "type": "speech_animation",
            "text": text,
            "emotion": emotion,
            "speed": speed
        }
        
        self.logger.info(f"Executing speech animation for text: {text[:50]}...")
        return {"status": "executed", "command": eva_command}
    
    async def simulate_motor_execution(self, command: Dict):
        """Simulate motor execution when Eva integration is disabled"""
        await asyncio.sleep(0.1)
        return {"status": "simulated", "command": command}
    
    async def coordinate_multiple_actions(self, actions: List[Dict]):
        """Coordinate multiple simultaneous actions"""
        try:
            coordination_tasks = []
            
            for action in actions:
                if self.validate_action_compatibility(action, actions):
                    task = asyncio.create_task(self.execute_motor_command(action))
                    coordination_tasks.append(task)
                else:
                    self.logger.warning(f"Action conflict detected: {action.get('type')}")
            
            results = await asyncio.gather(*coordination_tasks, return_exceptions=True)
            
            self.logger.info(f"Coordinated {len(results)} actions")
            return results
            
        except Exception as e:
            self.logger.error(f"Action coordination failed: {e}")
            return []
    
    def validate_action_compatibility(self, action: Dict, all_actions: List[Dict]) -> bool:
        """Validate that actions can be executed simultaneously"""
        action_type = action.get('type')
        
        conflicting_types = {
            'facial_expression': ['facial_expression'],
            'head_movement': ['head_movement'],
            'speech_animation': ['speech_animation']
        }
        
        conflicts = conflicting_types.get(action_type, [])
        
        for other_action in all_actions:
            if other_action != action and other_action.get('type') in conflicts:
                return False
        
        return True
    
    async def receive_from_cerebral_triad(self, message: Dict):
        """Receive motor commands from cerebral triad"""
        if self.membrane_controller.enforce_membrane_boundary(
            "cerebral_triad", "somatic_triad", message
        ):
            command = message.get('data', {})
            return await self.execute_motor_command(command)
        else:
            self.logger.warning("Membrane boundary prevented motor command reception")
            return {"status": "boundary_violation"}
