from flask import Flask, request, jsonify
from cognitive_core.shared_libraries.atomspace_manager import CognitiveAtomSpaceManager
from cognitive_core.shared_libraries.membrane_controller import MembraneController
from integration_hub.event_bus.event_bus import CognitiveEventBus
import logging
import asyncio
from typing import Dict, Any
import json

class CognitiveAPIGateway:
    def __init__(self):
        self.app = Flask(__name__)
        self.atomspace_manager = CognitiveAtomSpaceManager()
        self.membrane_controller = MembraneController(self.atomspace_manager.atomspace)
        self.event_bus = CognitiveEventBus()
        self.logger = logging.getLogger(__name__)
        
        self.triad_services = {
            "cerebral": {
                "thought_service": "http://localhost:8001",
                "processing_director": "http://localhost:8002",
                "processing_service": "http://localhost:8003",
                "output_service": "http://localhost:8004"
            },
            "somatic": {
                "motor_control_service": "http://localhost:8005",
                "sensory_service": "http://localhost:8006",
                "processing_service": "http://localhost:8007",
                "output_service": "http://localhost:8008"
            },
            "autonomic": {
                "monitoring_service": "http://localhost:8009",
                "state_management": "http://localhost:8010",
                "process_director": "http://localhost:8011",
                "processing_service": "http://localhost:8012",
                "trigger_service": "http://localhost:8013"
            }
        }
        
        self.setup_routes()
        
    def setup_routes(self):
        """Setup API routes for cognitive cities architecture"""
        
        @self.app.route('/api/v1/triad/<triad_name>/process', methods=['POST'])
        def process_triad_request(triad_name):
            """Route requests to appropriate triad services"""
            try:
                data = request.get_json()
                result = asyncio.run(self.route_to_triad(triad_name, data))
                return jsonify(result)
            except Exception as e:
                self.logger.error(f"Triad request processing failed: {e}")
                return jsonify({"error": str(e)}), 500
        
        @self.app.route('/api/v1/system5/component/<int:component_number>', methods=['GET'])
        def get_system5_component(component_number):
            """Get System-5 CNS component mapping"""
            try:
                mapping = self.get_component_mapping(component_number)
                return jsonify(mapping)
            except Exception as e:
                return jsonify({"error": str(e)}), 404
        
        @self.app.route('/api/v1/hemisphere/<hemisphere_type>/process', methods=['POST'])
        def process_hemisphere_request(hemisphere_type):
            """Process requests with hemisphere-specific routing"""
            try:
                data = request.get_json()
                result = asyncio.run(self.route_by_hemisphere(hemisphere_type, data))
                return jsonify(result)
            except Exception as e:
                return jsonify({"error": str(e)}), 500
        
        @self.app.route('/api/v1/polarity/<polarity_type>/trigger', methods=['POST'])
        def trigger_polarity_response(polarity_type):
            """Trigger polarity-specific responses in autonomic triad"""
            try:
                data = request.get_json()
                result = asyncio.run(self.trigger_polarity_response(polarity_type, data))
                return jsonify(result)
            except Exception as e:
                return jsonify({"error": str(e)}), 500
        
        @self.app.route('/api/v1/communication/pathway', methods=['POST'])
        def create_communication_pathway():
            """Create communication pathway between triads"""
            try:
                data = request.get_json()
                result = asyncio.run(self.create_pathway(data))
                return jsonify(result)
            except Exception as e:
                return jsonify({"error": str(e)}), 500
        
        @self.app.route('/api/v1/membrane/validate', methods=['POST'])
        def validate_membrane_boundary():
            """Validate membrane boundary crossing"""
            try:
                data = request.get_json()
                result = self.validate_boundary(data)
                return jsonify(result)
            except Exception as e:
                return jsonify({"error": str(e)}), 500
        
        @self.app.route('/api/v1/system/status', methods=['GET'])
        def get_system_status():
            """Get overall system status"""
            try:
                status = asyncio.run(self.get_system_status())
                return jsonify(status)
            except Exception as e:
                return jsonify({"error": str(e)}), 500
        
        @self.app.route('/api/v1/events/history', methods=['GET'])
        def get_event_history():
            """Get event communication history"""
            try:
                limit = request.args.get('limit', 100, type=int)
                history = self.event_bus.get_event_history(limit)
                return jsonify(history)
            except Exception as e:
                return jsonify({"error": str(e)}), 500
    
    async def route_to_triad(self, triad_name: str, data: Dict[str, Any]) -> Dict[str, Any]:
        """Route requests to appropriate triad services"""
        if triad_name not in self.triad_services:
            raise ValueError(f"Unknown triad: {triad_name}")
        
        service_name = data.get('service', 'processing_service')
        
        if service_name not in self.triad_services[triad_name]:
            raise ValueError(f"Unknown service {service_name} in triad {triad_name}")
        
        message = {
            "source": "api_gateway",
            "target": f"{triad_name}_triad",
            "service": service_name,
            "data": data
        }
        
        if self.membrane_controller.enforce_membrane_boundary(
            "integration_hub", f"{triad_name}_triad", message
        ):
            result = await self.forward_to_service(triad_name, service_name, data)
            return result
        else:
            raise PermissionError("Membrane boundary violation")
    
    async def forward_to_service(self, triad_name: str, service_name: str, data: Dict[str, Any]) -> Dict[str, Any]:
        """Forward request to specific service"""
        service_url = self.triad_services[triad_name][service_name]
        
        result = {
            "triad": triad_name,
            "service": service_name,
            "status": "processed",
            "data": data,
            "service_url": service_url
        }
        
        await self.event_bus.publish(f"{triad_name}.api_request", result)
        
        return result
    
    def get_component_mapping(self, component_number: int) -> Dict[str, Any]:
        """Get System-5 CNS component mapping"""
        component_mappings = {
            1: {
                "number": 1,
                "letter": "M",
                "services": ["motor_control_service", "monitoring_service"],
                "triads": ["somatic", "autonomic"],
                "function": "Motor control and monitoring"
            },
            2: {
                "number": 2,
                "letter": "PD",
                "services": ["processing_director", "process_director"],
                "triads": ["cerebral", "autonomic"],
                "function": "Processing direction and coordination"
            },
            5: {
                "number": 5,
                "letter": "P",
                "services": ["processing_service"],
                "triads": ["cerebral", "somatic", "autonomic"],
                "function": "Core processing across all triads"
            },
            7: {
                "number": 7,
                "letter": "T",
                "services": ["thought_service", "trigger_service"],
                "triads": ["cerebral", "autonomic"],
                "function": "Thought generation and trigger responses"
            },
            8: {
                "number": 8,
                "letter": "S",
                "services": ["sensory_service", "state_management"],
                "triads": ["somatic", "autonomic"],
                "function": "Sensory processing and state management"
            },
            4: {
                "number": 4,
                "letter": "O",
                "services": ["output_service"],
                "triads": ["cerebral", "somatic"],
                "function": "Output delivery and formatting"
            }
        }
        
        if component_number not in component_mappings:
            raise ValueError(f"Unknown component number: {component_number}")
        
        return component_mappings[component_number]
    
    async def route_by_hemisphere(self, hemisphere_type: str, data: Dict[str, Any]) -> Dict[str, Any]:
        """Route requests based on hemisphere type"""
        if hemisphere_type == "right":
            return await self.route_to_triad("cerebral", {
                "service": "thought_service",
                "hemisphere": "right",
                "processing_type": "intuitive",
                **data
            })
        elif hemisphere_type == "left":
            return await self.route_to_triad("cerebral", {
                "service": "output_service",
                "hemisphere": "left", 
                "processing_type": "analytical",
                **data
            })
        else:
            raise ValueError(f"Unknown hemisphere type: {hemisphere_type}")
    
    async def trigger_polarity_response(self, polarity_type: str, data: Dict[str, Any]) -> Dict[str, Any]:
        """Trigger polarity-specific responses"""
        if polarity_type == "sympathetic":
            return await self.route_to_triad("autonomic", {
                "service": "monitoring_service",
                "polarity": "sympathetic",
                "response_type": "active",
                **data
            })
        elif polarity_type == "parasympathetic":
            return await self.route_to_triad("autonomic", {
                "service": "trigger_service",
                "polarity": "parasympathetic",
                "response_type": "calming",
                **data
            })
        else:
            raise ValueError(f"Unknown polarity type: {polarity_type}")
    
    async def create_pathway(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Create communication pathway between triads"""
        source_triad = data.get('source_triad')
        target_triad = data.get('target_triad')
        event_type = data.get('event_type')
        pathway_data = data.get('data', {})
        
        success = await self.event_bus.publish_triad_event(
            source_triad, target_triad, event_type, pathway_data
        )
        
        if success:
            pathway_atom = self.atomspace_manager.create_communication_pathway(
                source_triad, target_triad, event_type
            )
            
            return {
                "status": "pathway_created",
                "source": source_triad,
                "target": target_triad,
                "event_type": event_type,
                "atom_id": str(pathway_atom)
            }
        else:
            return {"status": "pathway_failed"}
    
    def validate_boundary(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Validate membrane boundary crossing"""
        source_triad = data.get('source_triad')
        target_triad = data.get('target_triad')
        message = data.get('message', {})
        
        valid = self.membrane_controller.enforce_membrane_boundary(
            source_triad, target_triad, message
        )
        
        return {
            "valid": valid,
            "source": source_triad,
            "target": target_triad,
            "boundary_type": self.membrane_controller.get_membrane_type(target_triad)
        }
    
    async def get_system_status(self) -> Dict[str, Any]:
        """Get overall system status"""
        triad_stats = self.event_bus.get_triad_statistics()
        
        status = {
            "triads": {
                "cerebral": {
                    "status": "active",
                    "services": list(self.triad_services["cerebral"].keys()),
                    "communication_stats": triad_stats.get("cerebral", {})
                },
                "somatic": {
                    "status": "active",
                    "services": list(self.triad_services["somatic"].keys()),
                    "communication_stats": triad_stats.get("somatic", {})
                },
                "autonomic": {
                    "status": "active",
                    "services": list(self.triad_services["autonomic"].keys()),
                    "communication_stats": triad_stats.get("autonomic", {})
                }
            },
            "atomspace": {
                "atom_count": len(self.atomspace_manager.atomspace),
                "status": "connected"
            },
            "event_bus": {
                "active_subscriptions": len(self.event_bus.subscribers),
                "event_history_size": len(self.event_bus.event_history)
            }
        }
        
        return status
    
    def run(self, host='0.0.0.0', port=8000, debug=False):
        """Run the API gateway"""
        self.logger.info(f"Starting Cognitive API Gateway on {host}:{port}")
        self.app.run(host=host, port=port, debug=debug)
