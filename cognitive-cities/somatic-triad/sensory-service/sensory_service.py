from cognitive_core.shared_libraries.atomspace_manager import CognitiveAtomSpaceManager
from cognitive_core.shared_libraries.membrane_controller import MembraneController
from opencog.type_constructors import *
import logging
import asyncio
from typing import Dict, List, Any

class SensoryService:
    def __init__(self):
        self.atomspace_manager = CognitiveAtomSpaceManager()
        self.membrane_controller = MembraneController(self.atomspace_manager.atomspace)
        self.component_number = 8
        self.component_letter = "S"
        self.vision_enabled = True
        self.audio_enabled = True
        self.logger = logging.getLogger(__name__)
        
        self.setup_system5_mapping()
        self.setup_sensory_processing_rules()
        
    def setup_system5_mapping(self):
        """Map this service to System-5 CNS component S(8)"""
        self.atomspace_manager.store_system5_mapping(
            self.component_number,
            self.component_letter,
            "sensory_service",
            "somatic_triad"
        )
        
    def setup_sensory_processing_rules(self):
        """Setup sensory processing rules following Eva's pipeline"""
        context = EvaluationLink(
            PredicateNode("has_sensory_input"),
            VariableNode("$input")
        )
        
        action = ExecutionOutputLink(
            GroundedSchemaNode("py: process_sensory_input"),
            VariableNode("$input")
        )
        
        goal = ConceptNode("sensory_processing_completion")
        
        self.sensory_rule = self.membrane_controller.create_membrane_rule(
            context, action, goal, "somatic_triad"
        )
        
    async def process_sensory_input(self, raw_input: Dict):
        """Process sensory input following Eva's 5-step pipeline"""
        try:
            processed_data = await self.eva_sensory_pipeline(raw_input)
            
            self.logger.info(f"Processed sensory input: {raw_input.get('type', 'unknown')}")
            return processed_data
            
        except Exception as e:
            self.logger.error(f"Sensory processing failed: {e}")
            return {"status": "failed", "error": str(e)}
    
    async def eva_sensory_pipeline(self, raw_input: Dict):
        """Eva's 5-step sensory processing pipeline"""
        input_type = raw_input.get('type', 'unknown')
        
        if input_type == 'vision':
            return await self.process_vision_input(raw_input)
        elif input_type == 'audio':
            return await self.process_audio_input(raw_input)
        elif input_type == 'text':
            return await self.process_text_input(raw_input)
        else:
            return await self.process_generic_input(raw_input)
    
    async def process_vision_input(self, vision_data: Dict):
        """Process visual sensory input"""
        if not self.vision_enabled:
            return {"status": "vision_disabled"}
        
        processed = {
            "type": "vision",
            "faces_detected": await self.detect_faces(vision_data),
            "objects_detected": await self.detect_objects(vision_data),
            "movement_detected": await self.detect_movement(vision_data),
            "timestamp": vision_data.get('timestamp'),
            "confidence": 0.8
        }
        
        await self.store_vision_atoms(processed)
        return processed
    
    async def detect_faces(self, vision_data: Dict) -> List[Dict]:
        """Detect faces in visual input"""
        faces = []
        
        face_data = vision_data.get('faces', [])
        for face in face_data:
            face_info = {
                "id": face.get('id', 'unknown'),
                "position": face.get('position', [0, 0]),
                "size": face.get('size', [0, 0]),
                "emotion": face.get('emotion', 'neutral'),
                "known": face.get('known', False)
            }
            faces.append(face_info)
        
        self.logger.info(f"Detected {len(faces)} faces")
        return faces
    
    async def detect_objects(self, vision_data: Dict) -> List[Dict]:
        """Detect objects in visual input"""
        objects = []
        
        object_data = vision_data.get('objects', [])
        for obj in object_data:
            object_info = {
                "type": obj.get('type', 'unknown'),
                "position": obj.get('position', [0, 0]),
                "size": obj.get('size', [0, 0]),
                "confidence": obj.get('confidence', 0.5)
            }
            objects.append(object_info)
        
        self.logger.info(f"Detected {len(objects)} objects")
        return objects
    
    async def detect_movement(self, vision_data: Dict) -> Dict:
        """Detect movement in visual input"""
        movement = vision_data.get('movement', {})
        
        movement_info = {
            "detected": movement.get('detected', False),
            "direction": movement.get('direction', 'none'),
            "intensity": movement.get('intensity', 0.0),
            "objects_moving": movement.get('objects_moving', [])
        }
        
        return movement_info
    
    async def process_audio_input(self, audio_data: Dict):
        """Process audio sensory input"""
        if not self.audio_enabled:
            return {"status": "audio_disabled"}
        
        processed = {
            "type": "audio",
            "speech_detected": await self.detect_speech(audio_data),
            "sound_level": await self.measure_sound_level(audio_data),
            "sound_classification": await self.classify_sounds(audio_data),
            "timestamp": audio_data.get('timestamp'),
            "confidence": 0.7
        }
        
        await self.store_audio_atoms(processed)
        return processed
    
    async def detect_speech(self, audio_data: Dict) -> Dict:
        """Detect speech in audio input"""
        speech_data = audio_data.get('speech', {})
        
        speech_info = {
            "detected": speech_data.get('detected', False),
            "text": speech_data.get('text', ''),
            "speaker_id": speech_data.get('speaker_id', 'unknown'),
            "emotion": speech_data.get('emotion', 'neutral'),
            "confidence": speech_data.get('confidence', 0.5)
        }
        
        return speech_info
    
    async def measure_sound_level(self, audio_data: Dict) -> float:
        """Measure sound level in audio input"""
        return audio_data.get('volume', 0.0)
    
    async def classify_sounds(self, audio_data: Dict) -> List[str]:
        """Classify sounds in audio input"""
        return audio_data.get('sound_types', [])
    
    async def process_text_input(self, text_data: Dict):
        """Process text sensory input"""
        processed = {
            "type": "text",
            "text": text_data.get('text', ''),
            "language": await self.detect_language(text_data),
            "sentiment": await self.analyze_sentiment(text_data),
            "entities": await self.extract_entities(text_data),
            "timestamp": text_data.get('timestamp'),
            "confidence": 0.9
        }
        
        await self.store_text_atoms(processed)
        return processed
    
    async def detect_language(self, text_data: Dict) -> str:
        """Detect language of text input"""
        return text_data.get('language', 'en')
    
    async def analyze_sentiment(self, text_data: Dict) -> Dict:
        """Analyze sentiment of text input"""
        return {
            "polarity": text_data.get('sentiment_polarity', 0.0),
            "subjectivity": text_data.get('sentiment_subjectivity', 0.0)
        }
    
    async def extract_entities(self, text_data: Dict) -> List[Dict]:
        """Extract entities from text input"""
        return text_data.get('entities', [])
    
    async def process_generic_input(self, input_data: Dict):
        """Process generic sensory input"""
        processed = {
            "type": input_data.get('type', 'generic'),
            "data": input_data.get('data', {}),
            "timestamp": input_data.get('timestamp'),
            "confidence": 0.6
        }
        
        return processed
    
    async def store_vision_atoms(self, vision_data: Dict):
        """Store vision data as atoms in AtomSpace"""
        for face in vision_data.get('faces_detected', []):
            face_node = ConceptNode(f"face_{face['id']}")
            emotion_node = ConceptNode(face['emotion'])
            
            self.atomspace_manager.atomspace.add_link(
                types.EvaluationLink,
                [
                    PredicateNode("has_emotion"),
                    ListLink(face_node, emotion_node)
                ]
            )
    
    async def store_audio_atoms(self, audio_data: Dict):
        """Store audio data as atoms in AtomSpace"""
        speech = audio_data.get('speech_detected', {})
        if speech.get('detected'):
            speech_node = ConceptNode("current_speech")
            text_node = ConceptNode(speech['text'])
            
            self.atomspace_manager.atomspace.add_link(
                types.EvaluationLink,
                [
                    PredicateNode("contains_text"),
                    ListLink(speech_node, text_node)
                ]
            )
    
    async def store_text_atoms(self, text_data: Dict):
        """Store text data as atoms in AtomSpace"""
        text_node = ConceptNode("current_text")
        content_node = ConceptNode(text_data['text'])
        
        self.atomspace_manager.atomspace.add_link(
            types.EvaluationLink,
            [
                PredicateNode("has_content"),
                ListLink(text_node, content_node)
            ]
        )
    
    async def send_to_cerebral_triad(self, processed_data: Dict):
        """Send processed sensory data to cerebral triad"""
        message = {
            "source": "sensory_service",
            "target": "cerebral_triad",
            "type": "sensory_data",
            "data": processed_data
        }
        
        if self.membrane_controller.enforce_membrane_boundary(
            "somatic_triad", "cerebral_triad", message
        ):
            self.logger.info("Sensory data sent to cerebral triad")
            return True
        else:
            self.logger.warning("Membrane boundary prevented sensory data transmission")
            return False
