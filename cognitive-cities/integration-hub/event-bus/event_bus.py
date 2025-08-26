import asyncio
from typing import Dict, List, Callable, Any
import logging
import json
from datetime import datetime

class CognitiveEventBus:
    def __init__(self):
        self.subscribers: Dict[str, List[Callable]] = {}
        self.event_history: List[Dict] = []
        self.logger = logging.getLogger(__name__)
        
        self.triad_connections = {
            "cerebral_to_somatic": [
                "action_commands", 
                "behavioral_requests", 
                "motor_instructions",
                "output_delivery"
            ],
            "cerebral_to_autonomic": [
                "emotional_states", 
                "attention_allocation",
                "cognitive_load",
                "processing_status"
            ],
            "somatic_to_autonomic": [
                "stress_indicators", 
                "sensory_overload",
                "motor_feedback",
                "behavioral_status"
            ],
            "somatic_to_cerebral": [
                "sensory_data",
                "environmental_input",
                "feedback_signals",
                "status_updates"
            ],
            "autonomic_to_cerebral": [
                "system_alerts",
                "resource_status",
                "emotional_regulation",
                "attention_requests"
            ],
            "autonomic_to_somatic": [
                "regulatory_signals",
                "stress_responses",
                "energy_management",
                "system_constraints"
            ]
        }
        
    async def publish_triad_event(self, source_triad: str, target_triad: str, event_type: str, data: Any):
        """Publish events between triads following CNS pathways"""
        connection_key = f"{source_triad}_to_{target_triad}"
        
        if connection_key not in self.triad_connections:
            self.logger.error(f"Invalid triad connection: {connection_key}")
            return False
            
        if event_type not in self.triad_connections[connection_key]:
            self.logger.warning(f"Event type {event_type} not allowed for connection {connection_key}")
            return False
        
        event = {
            "source_triad": source_triad,
            "target_triad": target_triad,
            "event_type": event_type,
            "data": data,
            "timestamp": datetime.utcnow().isoformat(),
            "connection_pathway": connection_key
        }
        
        topic = f"{target_triad}.{event_type}"
        success = await self.publish(topic, event)
        
        if success:
            self.event_history.append(event)
            self.logger.info(f"Published triad event: {source_triad} -> {target_triad} ({event_type})")
        
        return success
        
    async def publish(self, topic: str, data: Any) -> bool:
        """Publish event to topic subscribers"""
        try:
            if topic in self.subscribers:
                tasks = []
                for callback in self.subscribers[topic]:
                    task = asyncio.create_task(self._safe_callback(callback, data))
                    tasks.append(task)
                
                if tasks:
                    await asyncio.gather(*tasks, return_exceptions=True)
                    
                self.logger.debug(f"Published to {len(tasks)} subscribers on topic: {topic}")
                return True
            else:
                self.logger.debug(f"No subscribers for topic: {topic}")
                return True
                
        except Exception as e:
            self.logger.error(f"Failed to publish to topic {topic}: {e}")
            return False
    
    async def _safe_callback(self, callback: Callable, data: Any):
        """Safely execute callback with error handling"""
        try:
            if asyncio.iscoroutinefunction(callback):
                await callback(data)
            else:
                callback(data)
        except Exception as e:
            self.logger.error(f"Callback execution failed: {e}")
    
    def subscribe(self, topic: str, callback: Callable):
        """Subscribe to topic events"""
        if topic not in self.subscribers:
            self.subscribers[topic] = []
        
        self.subscribers[topic].append(callback)
        self.logger.info(f"Subscribed to topic: {topic}")
        
    def unsubscribe(self, topic: str, callback: Callable):
        """Unsubscribe from topic events"""
        if topic in self.subscribers and callback in self.subscribers[topic]:
            self.subscribers[topic].remove(callback)
            self.logger.info(f"Unsubscribed from topic: {topic}")
            
            if not self.subscribers[topic]:
                del self.subscribers[topic]
    
    async def publish_cerebral_to_somatic(self, event_type: str, data: Any):
        """Publish event from cerebral to somatic triad"""
        return await self.publish_triad_event("cerebral", "somatic", event_type, data)
    
    async def publish_cerebral_to_autonomic(self, event_type: str, data: Any):
        """Publish event from cerebral to autonomic triad"""
        return await self.publish_triad_event("cerebral", "autonomic", event_type, data)
    
    async def publish_somatic_to_cerebral(self, event_type: str, data: Any):
        """Publish event from somatic to cerebral triad"""
        return await self.publish_triad_event("somatic", "cerebral", event_type, data)
    
    async def publish_somatic_to_autonomic(self, event_type: str, data: Any):
        """Publish event from somatic to autonomic triad"""
        return await self.publish_triad_event("somatic", "autonomic", event_type, data)
    
    async def publish_autonomic_to_cerebral(self, event_type: str, data: Any):
        """Publish event from autonomic to cerebral triad"""
        return await self.publish_triad_event("autonomic", "cerebral", event_type, data)
    
    async def publish_autonomic_to_somatic(self, event_type: str, data: Any):
        """Publish event from autonomic to somatic triad"""
        return await self.publish_triad_event("autonomic", "somatic", event_type, data)
    
    def get_connection_pathways(self) -> Dict[str, List[str]]:
        """Get all available connection pathways between triads"""
        return self.triad_connections.copy()
    
    def get_event_history(self, limit: int = 100) -> List[Dict]:
        """Get recent event history"""
        return self.event_history[-limit:]
    
    def get_triad_statistics(self) -> Dict[str, Dict]:
        """Get statistics for triad communications"""
        stats = {
            "cerebral": {"sent": 0, "received": 0},
            "somatic": {"sent": 0, "received": 0},
            "autonomic": {"sent": 0, "received": 0}
        }
        
        for event in self.event_history:
            source = event["source_triad"]
            target = event["target_triad"]
            
            if source in stats:
                stats[source]["sent"] += 1
            if target in stats:
                stats[target]["received"] += 1
        
        return stats
    
    async def validate_triad_communication(self, source: str, target: str, event_type: str) -> bool:
        """Validate if communication between triads is allowed"""
        connection_key = f"{source}_to_{target}"
        
        if connection_key not in self.triad_connections:
            return False
            
        return event_type in self.triad_connections[connection_key]
    
    def clear_event_history(self):
        """Clear event history"""
        self.event_history.clear()
        self.logger.info("Event history cleared")
    
    async def broadcast_system_event(self, event_type: str, data: Any):
        """Broadcast system-wide event to all triads"""
        tasks = []
        
        for triad in ["cerebral", "somatic", "autonomic"]:
            topic = f"{triad}.{event_type}"
            task = asyncio.create_task(self.publish(topic, data))
            tasks.append(task)
        
        results = await asyncio.gather(*tasks, return_exceptions=True)
        success_count = sum(1 for result in results if result is True)
        
        self.logger.info(f"Broadcast system event to {success_count}/3 triads: {event_type}")
        return success_count == 3
