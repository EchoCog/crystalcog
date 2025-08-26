import unittest
import asyncio
from cognitive_core.shared_libraries.atomspace_manager import CognitiveAtomSpaceManager
from cognitive_core.shared_libraries.membrane_controller import MembraneController
from integration_hub.event_bus.event_bus import CognitiveEventBus
from cerebral_triad.thought_service.thought_service import ThoughtService
from somatic_triad.motor_control_service.motor_control import MotorControlService
from autonomic_triad.monitoring_service.monitoring_service import MonitoringService

class TestTriadIntegration(unittest.TestCase):
    def setUp(self):
        """Setup test environment"""
        self.atomspace_manager = CognitiveAtomSpaceManager()
        self.membrane_controller = MembraneController(self.atomspace_manager.atomspace)
        self.event_bus = CognitiveEventBus()
        
        self.thought_service = ThoughtService()
        self.motor_control_service = MotorControlService()
        self.monitoring_service = MonitoringService()
        
    def test_system5_component_mapping(self):
        """Test System-5 CNS component mapping"""
        thought_mapping = self.atomspace_manager.store_system5_mapping(
            7, "T", "thought_service", "cerebral_triad"
        )
        self.assertIsNotNone(thought_mapping)
        self.assertEqual(len(thought_mapping), 3)
        
        motor_mapping = self.atomspace_manager.store_system5_mapping(
            1, "M", "motor_control_service", "somatic_triad"
        )
        self.assertIsNotNone(motor_mapping)
        
    def test_cerebral_to_somatic_communication(self):
        """Test communication pathway from cerebral to somatic triad"""
        async def test_communication():
            success = await self.event_bus.publish_cerebral_to_somatic(
                "action_commands",
                {"command": "move_head", "direction": "left"}
            )
            self.assertTrue(success)
            
            history = self.event_bus.get_event_history(1)
            self.assertEqual(len(history), 1)
            self.assertEqual(history[0]["source_triad"], "cerebral")
            self.assertEqual(history[0]["target_triad"], "somatic")
        
        asyncio.run(test_communication())
        
    def test_cerebral_to_autonomic_communication(self):
        """Test communication pathway from cerebral to autonomic triad"""
        async def test_communication():
            success = await self.event_bus.publish_cerebral_to_autonomic(
                "emotional_states",
                {"emotion": "calm", "intensity": 0.7}
            )
            self.assertTrue(success)
        
        asyncio.run(test_communication())
        
    def test_somatic_to_autonomic_communication(self):
        """Test communication pathway from somatic to autonomic triad"""
        async def test_communication():
            success = await self.event_bus.publish_somatic_to_autonomic(
                "stress_indicators",
                {"stress_level": 0.8, "source": "sensory_overload"}
            )
            self.assertTrue(success)
        
        asyncio.run(test_communication())
        
    def test_hemisphere_distinction(self):
        """Test right hemisphere (intuitive) vs left hemisphere (applied technique)"""
        right_hemisphere_atom = self.atomspace_manager.create_hemisphere_atom(
            "right", "intuitive_processing"
        )
        self.assertIsNotNone(right_hemisphere_atom)
        
        left_hemisphere_atom = self.atomspace_manager.create_hemisphere_atom(
            "left", "analytical_processing"
        )
        self.assertIsNotNone(left_hemisphere_atom)
        
        self.assertEqual(self.thought_service.hemisphere, "right")
        
    def test_polarity_distinction(self):
        """Test sympathetic vs parasympathetic polarity in autonomic triad"""
        sympathetic_atom = self.atomspace_manager.create_polarity_atom(
            "sympathetic", "monitoring_function"
        )
        self.assertIsNotNone(sympathetic_atom)
        
        parasympathetic_atom = self.atomspace_manager.create_polarity_atom(
            "parasympathetic", "trigger_function"
        )
        self.assertIsNotNone(parasympathetic_atom)
        
        self.assertEqual(self.monitoring_service.polarity, "sympathetic")
        
    def test_membrane_boundary_enforcement(self):
        """Test P-System membrane boundary enforcement"""
        valid_message = {
            "source": "cerebral_triad",
            "target": "somatic_triad",
            "type": "action_command",
            "data": {"action": "test"}
        }
        
        boundary_valid = self.membrane_controller.enforce_membrane_boundary(
            "cerebral_triad", "somatic_triad", valid_message
        )
        self.assertTrue(boundary_valid)
        
    def test_invalid_triad_communication(self):
        """Test that invalid triad communications are rejected"""
        async def test_invalid():
            success = await self.event_bus.publish_triad_event(
                "cerebral", "somatic", "invalid_event_type", {}
            )
            self.assertFalse(success)
        
        asyncio.run(test_invalid())
        
    def test_thought_service_intuitive_processing(self):
        """Test thought service intuitive idea generation"""
        async def test_intuitive():
            ideas = await self.thought_service.generate_intuitive_ideas(
                {"text": "create something beautiful"}
            )
            self.assertIsInstance(ideas, list)
        
        asyncio.run(test_intuitive())
        
    def test_motor_control_eva_integration(self):
        """Test motor control service Eva integration"""
        async def test_motor():
            command = {
                "type": "facial_expression",
                "expression": "smile",
                "intensity": 0.8
            }
            result = await self.motor_control_service.execute_motor_command(command)
            self.assertEqual(result["status"], "executed")
        
        asyncio.run(test_motor())
        
    def test_monitoring_service_sympathetic_response(self):
        """Test monitoring service sympathetic nervous system response"""
        async def test_monitoring():
            metrics = await self.monitoring_service.monitor_system_health()
            self.assertIn("cpu_usage", metrics)
            self.assertIn("memory_usage", metrics)
            self.assertEqual(metrics["polarity"], "sympathetic")
        
        asyncio.run(test_monitoring())
        
    def test_triad_component_retrieval(self):
        """Test retrieval of triad components from AtomSpace"""
        components = self.atomspace_manager.get_triad_components("cerebral_triad")
        self.assertIsInstance(components, list)
        
    def test_communication_pathway_creation(self):
        """Test creation of communication pathways between triads"""
        pathway = self.atomspace_manager.create_communication_pathway(
            "cerebral_triad", "somatic_triad", "action_coordination"
        )
        self.assertIsNotNone(pathway)
        
    def test_event_bus_statistics(self):
        """Test event bus communication statistics"""
        async def test_stats():
            await self.event_bus.publish_cerebral_to_somatic("action_commands", {})
            await self.event_bus.publish_somatic_to_cerebral("sensory_data", {})
            
            stats = self.event_bus.get_triad_statistics()
            self.assertIn("cerebral", stats)
            self.assertIn("somatic", stats)
            self.assertGreater(stats["cerebral"]["sent"], 0)
            self.assertGreater(stats["somatic"]["received"], 0)
        
        asyncio.run(test_stats())
        
    def tearDown(self):
        """Clean up test environment"""
        self.event_bus.clear_event_history()

if __name__ == '__main__':
    unittest.main()
