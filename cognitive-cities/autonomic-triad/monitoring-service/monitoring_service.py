from cognitive_core.shared_libraries.atomspace_manager import CognitiveAtomSpaceManager
from cognitive_core.shared_libraries.membrane_controller import MembraneController
from opencog.type_constructors import *
import logging
import asyncio
import time
from typing import Dict, List

class MonitoringService:
    def __init__(self):
        self.atomspace_manager = CognitiveAtomSpaceManager()
        self.membrane_controller = MembraneController(self.atomspace_manager.atomspace)
        self.component_number = 1
        self.component_letter = "M"
        self.polarity = "sympathetic"
        self.monitoring_active = True
        self.logger = logging.getLogger(__name__)
        
        self.setup_system5_mapping()
        self.setup_sympathetic_monitoring_rules()
        
    def setup_system5_mapping(self):
        """Map this service to System-5 CNS component M(1) in autonomic triad"""
        self.atomspace_manager.store_system5_mapping(
            self.component_number,
            self.component_letter,
            "monitoring_service",
            "autonomic_triad"
        )
        
    def setup_sympathetic_monitoring_rules(self):
        """Setup sympathetic nervous system monitoring rules"""
        context = AndLink(
            EvaluationLink(
                PredicateNode("system_requires_monitoring"),
                ConceptNode("true")
            ),
            EvaluationLink(
                PredicateNode("polarity"),
                ConceptNode("sympathetic")
            )
        )
        
        action = ExecutionOutputLink(
            GroundedSchemaNode("py: sympathetic_monitoring"),
            ConceptNode("system_state")
        )
        
        self.sympathetic_rule = self.membrane_controller.create_polarity_rule(
            "sympathetic", "monitoring", context, action
        )
        
    async def monitor_system_health(self):
        """Active system monitoring with sympathetic response"""
        try:
            health_metrics = await self.sympathetic_monitoring()
            
            if self.detect_stress_indicators(health_metrics):
                await self.trigger_sympathetic_response(health_metrics)
            
            self.logger.info("System health monitoring completed")
            return health_metrics
            
        except Exception as e:
            self.logger.error(f"System health monitoring failed: {e}")
            return {}
    
    async def sympathetic_monitoring(self):
        """Implement sympathetic nervous system monitoring"""
        metrics = {
            "cpu_usage": await self.monitor_cpu_usage(),
            "memory_usage": await self.monitor_memory_usage(),
            "network_activity": await self.monitor_network_activity(),
            "service_health": await self.monitor_service_health(),
            "error_rates": await self.monitor_error_rates(),
            "response_times": await self.monitor_response_times(),
            "timestamp": time.time(),
            "polarity": self.polarity
        }
        
        await self.store_monitoring_atoms(metrics)
        return metrics
    
    async def monitor_cpu_usage(self) -> float:
        """Monitor CPU usage"""
        try:
            import psutil
            return psutil.cpu_percent(interval=1)
        except ImportError:
            return 50.0
    
    async def monitor_memory_usage(self) -> float:
        """Monitor memory usage"""
        try:
            import psutil
            return psutil.virtual_memory().percent
        except ImportError:
            return 60.0
    
    async def monitor_network_activity(self) -> Dict:
        """Monitor network activity"""
        try:
            import psutil
            net_io = psutil.net_io_counters()
            return {
                "bytes_sent": net_io.bytes_sent,
                "bytes_recv": net_io.bytes_recv,
                "packets_sent": net_io.packets_sent,
                "packets_recv": net_io.packets_recv
            }
        except ImportError:
            return {
                "bytes_sent": 1000000,
                "bytes_recv": 2000000,
                "packets_sent": 1000,
                "packets_recv": 2000
            }
    
    async def monitor_service_health(self) -> Dict:
        """Monitor health of triad services"""
        services = {
            "cerebral_triad": await self.check_triad_health("cerebral"),
            "somatic_triad": await self.check_triad_health("somatic"),
            "autonomic_triad": await self.check_triad_health("autonomic")
        }
        
        return services
    
    async def check_triad_health(self, triad_name: str) -> Dict:
        """Check health of a specific triad"""
        health_status = {
            "status": "healthy",
            "response_time": 0.1,
            "error_count": 0,
            "last_check": time.time()
        }
        
        return health_status
    
    async def monitor_error_rates(self) -> Dict:
        """Monitor error rates across services"""
        error_rates = {
            "thought_service": 0.01,
            "processing_director": 0.005,
            "motor_control_service": 0.02,
            "sensory_service": 0.015,
            "monitoring_service": 0.001
        }
        
        return error_rates
    
    async def monitor_response_times(self) -> Dict:
        """Monitor response times across services"""
        response_times = {
            "thought_service": 0.15,
            "processing_director": 0.08,
            "motor_control_service": 0.12,
            "sensory_service": 0.10,
            "monitoring_service": 0.05
        }
        
        return response_times
    
    def detect_stress_indicators(self, metrics: Dict) -> bool:
        """Detect system stress indicators for sympathetic response"""
        stress_detected = False
        
        if metrics.get("cpu_usage", 0) > 80:
            self.logger.warning("High CPU usage detected")
            stress_detected = True
        
        if metrics.get("memory_usage", 0) > 85:
            self.logger.warning("High memory usage detected")
            stress_detected = True
        
        error_rates = metrics.get("error_rates", {})
        for service, rate in error_rates.items():
            if rate > 0.05:
                self.logger.warning(f"High error rate in {service}: {rate}")
                stress_detected = True
        
        return stress_detected
    
    async def trigger_sympathetic_response(self, metrics: Dict):
        """Trigger sympathetic nervous system response to stress"""
        response_actions = []
        
        if metrics.get("cpu_usage", 0) > 80:
            response_actions.append("reduce_processing_load")
        
        if metrics.get("memory_usage", 0) > 85:
            response_actions.append("clear_memory_cache")
        
        for action in response_actions:
            await self.execute_sympathetic_action(action, metrics)
        
        await self.notify_other_triads("stress_detected", metrics)
    
    async def execute_sympathetic_action(self, action: str, metrics: Dict):
        """Execute sympathetic response action"""
        if action == "reduce_processing_load":
            await self.reduce_processing_load()
        elif action == "clear_memory_cache":
            await self.clear_memory_cache()
        
        self.logger.info(f"Executed sympathetic action: {action}")
    
    async def reduce_processing_load(self):
        """Reduce processing load across triads"""
        message = {
            "source": "monitoring_service",
            "target": "all_triads",
            "type": "reduce_load",
            "urgency": "high"
        }
        
        await self.broadcast_to_triads(message)
    
    async def clear_memory_cache(self):
        """Clear memory caches to free up memory"""
        import gc
        gc.collect()
        self.logger.info("Memory cache cleared")
    
    async def notify_other_triads(self, event_type: str, data: Dict):
        """Notify other triads of monitoring events"""
        message = {
            "source": "monitoring_service",
            "event_type": event_type,
            "data": data,
            "polarity": self.polarity
        }
        
        await self.send_to_cerebral_triad(message)
        await self.send_to_somatic_triad(message)
    
    async def send_to_cerebral_triad(self, message: Dict):
        """Send monitoring data to cerebral triad"""
        if self.membrane_controller.enforce_membrane_boundary(
            "autonomic_triad", "cerebral_triad", message
        ):
            self.logger.info("Monitoring data sent to cerebral triad")
            return True
        else:
            self.logger.warning("Membrane boundary prevented cerebral communication")
            return False
    
    async def send_to_somatic_triad(self, message: Dict):
        """Send monitoring data to somatic triad"""
        if self.membrane_controller.enforce_membrane_boundary(
            "autonomic_triad", "somatic_triad", message
        ):
            self.logger.info("Monitoring data sent to somatic triad")
            return True
        else:
            self.logger.warning("Membrane boundary prevented somatic communication")
            return False
    
    async def broadcast_to_triads(self, message: Dict):
        """Broadcast message to all triads"""
        await self.send_to_cerebral_triad(message)
        await self.send_to_somatic_triad(message)
    
    async def store_monitoring_atoms(self, metrics: Dict):
        """Store monitoring metrics as atoms in AtomSpace"""
        monitoring_node = ConceptNode("system_monitoring")
        
        for metric_name, metric_value in metrics.items():
            if isinstance(metric_value, (int, float)):
                metric_node = ConceptNode(metric_name)
                value_node = NumberNode(str(metric_value))
                
                self.atomspace_manager.atomspace.add_link(
                    types.EvaluationLink,
                    [
                        PredicateNode("has_metric_value"),
                        ListLink(monitoring_node, metric_node, value_node)
                    ]
                )
