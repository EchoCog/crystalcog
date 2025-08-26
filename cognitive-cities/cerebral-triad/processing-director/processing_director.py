from cognitive_core.shared_libraries.atomspace_manager import CognitiveAtomSpaceManager
from cognitive_core.shared_libraries.membrane_controller import MembraneController
from opencog.openpsi import *
from opencog.attention import *
from opencog.type_constructors import *
import logging
import asyncio
from typing import List, Dict

class ProcessingDirector:
    def __init__(self):
        self.atomspace_manager = CognitiveAtomSpaceManager()
        self.membrane_controller = MembraneController(self.atomspace_manager.atomspace)
        self.component_number = 2
        self.component_letter = "PD"
        self.ecan_enabled = True
        self.logger = logging.getLogger(__name__)
        
        self.setup_system5_mapping()
        self.setup_coordination_rules()
        
    def setup_system5_mapping(self):
        """Map this service to System-5 CNS component PD(2)"""
        self.atomspace_manager.store_system5_mapping(
            self.component_number,
            self.component_letter,
            "processing_director",
            "cerebral_triad"
        )
        
    def setup_coordination_rules(self):
        """Setup coordination rules for processing management"""
        context = AndLink(
            EvaluationLink(
                PredicateNode("has_processing_request"),
                VariableNode("$request")
            ),
            EvaluationLink(
                PredicateNode("resource_available"),
                VariableNode("$resource")
            )
        )
        
        action = ExecutionOutputLink(
            GroundedSchemaNode("py: coordinate_processing"),
            ListLink(VariableNode("$request"), VariableNode("$resource"))
        )
        
        goal = ConceptNode("efficient_processing_coordination")
        
        self.coordination_rule = self.membrane_controller.create_membrane_rule(
            context, action, goal, "cerebral_triad"
        )
        
    async def coordinate_processing(self, task_queue: List[Dict]):
        """Coordinate processing across cerebral triad using ECAN attention"""
        try:
            coordinated_tasks = []
            
            for task in task_queue:
                if self.ecan_enabled:
                    attention_allocated = await self.allocate_attention(task)
                    if attention_allocated:
                        routed_task = await self.route_to_appropriate_service(task)
                        coordinated_tasks.append(routed_task)
                else:
                    routed_task = await self.route_to_appropriate_service(task)
                    coordinated_tasks.append(routed_task)
            
            self.logger.info(f"Coordinated {len(coordinated_tasks)} tasks")
            return coordinated_tasks
            
        except Exception as e:
            self.logger.error(f"Processing coordination failed: {e}")
            return []
    
    async def allocate_attention(self, task: Dict) -> bool:
        """Use ECAN to allocate attention to processing tasks"""
        try:
            task_node = ConceptNode(f"task_{task.get('id', 'unknown')}")
            
            importance_value = self.calculate_task_importance(task)
            
            task_node.sti = importance_value
            
            if importance_value > 50:
                self.logger.info(f"High attention allocated to task {task.get('id')}")
                return True
            else:
                self.logger.info(f"Low attention allocated to task {task.get('id')}")
                return False
                
        except Exception as e:
            self.logger.error(f"Attention allocation failed: {e}")
            return False
    
    def calculate_task_importance(self, task: Dict) -> int:
        """Calculate importance value for ECAN attention allocation"""
        base_importance = 30
        
        if task.get('priority') == 'high':
            base_importance += 40
        elif task.get('priority') == 'medium':
            base_importance += 20
        
        if task.get('source') == 'thought_service':
            base_importance += 15
        
        if task.get('hemisphere') == 'right':
            base_importance += 10
        
        return min(base_importance, 100)
    
    async def route_to_appropriate_service(self, task: Dict) -> Dict:
        """Route tasks to appropriate cerebral triad services"""
        task_type = task.get('type', 'unknown')
        
        if task_type == 'idea_generation':
            return await self.route_to_thought_service(task)
        elif task_type == 'analytical_processing':
            return await self.route_to_processing_service(task)
        elif task_type == 'output_formatting':
            return await self.route_to_output_service(task)
        else:
            self.logger.warning(f"Unknown task type: {task_type}")
            return task
    
    async def route_to_thought_service(self, task: Dict) -> Dict:
        """Route task to thought service"""
        message = {
            "source": "processing_director",
            "target": "thought_service", 
            "task": task,
            "routing_decision": "idea_generation_required"
        }
        
        if self.membrane_controller.enforce_membrane_boundary(
            "cerebral_triad", "cerebral_triad", message
        ):
            task['routed_to'] = 'thought_service'
            task['status'] = 'routed'
            return task
        else:
            task['status'] = 'routing_failed'
            return task
    
    async def route_to_processing_service(self, task: Dict) -> Dict:
        """Route task to processing service"""
        message = {
            "source": "processing_director",
            "target": "processing_service",
            "task": task,
            "routing_decision": "analytical_processing_required"
        }
        
        if self.membrane_controller.enforce_membrane_boundary(
            "cerebral_triad", "cerebral_triad", message
        ):
            task['routed_to'] = 'processing_service'
            task['status'] = 'routed'
            return task
        else:
            task['status'] = 'routing_failed'
            return task
    
    async def route_to_output_service(self, task: Dict) -> Dict:
        """Route task to output service"""
        message = {
            "source": "processing_director",
            "target": "output_service",
            "task": task,
            "routing_decision": "output_formatting_required"
        }
        
        if self.membrane_controller.enforce_membrane_boundary(
            "cerebral_triad", "cerebral_triad", message
        ):
            task['routed_to'] = 'output_service'
            task['status'] = 'routed'
            return task
        else:
            task['status'] = 'routing_failed'
            return task
    
    async def monitor_processing_load(self):
        """Monitor processing load across cerebral triad services"""
        try:
            load_metrics = {
                'thought_service': await self.get_service_load('thought_service'),
                'processing_service': await self.get_service_load('processing_service'),
                'output_service': await self.get_service_load('output_service')
            }
            
            self.logger.info(f"Processing load metrics: {load_metrics}")
            return load_metrics
            
        except Exception as e:
            self.logger.error(f"Load monitoring failed: {e}")
            return {}
    
    async def get_service_load(self, service_name: str) -> float:
        """Get current load for a specific service"""
        try:
            service_node = ConceptNode(service_name)
            load_value = service_node.sti / 100.0
            return min(load_value, 1.0)
        except:
            return 0.5
