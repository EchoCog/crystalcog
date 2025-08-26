# Somatic Triad

## Overview

The Somatic Triad handles voluntary operations and behavior execution within the Cognitive Cities Architecture. It corresponds to the Extension Membrane in the P-System Membrane Architecture and manages all external interactions and behavioral responses.

## Color Code: Light Blue

The Somatic Triad is represented by light blue in the neurological metaphor, symbolizing conscious movement, sensory processing, and voluntary actions.

## Services

### 1. Motor Control Service
- **Purpose**: Coordinates actions and behaviors
- **Integration**: Connects to Eva animation system and robot control
- **Input**: Action commands from Cerebral Triad, behavioral patterns
- **Output**: Motor commands, animation sequences, physical actions

### 2. Sensory Service
- **Purpose**: Collects and processes external inputs
- **Integration**: Interfaces with vision, audio, and other sensory systems
- **Input**: Raw sensory data (video, audio, text, sensor readings)
- **Output**: Processed sensory information, feature extraction

### 3. Processing Service
- **Purpose**: Handles behavioral technique implementation
- **Integration**: Uses existing OpenCog behavioral processing modules
- **Input**: Behavioral requests, context information
- **Output**: Behavioral strategies, execution plans

### 4. Output Service
- **Purpose**: Delivers behavioral responses
- **Integration**: Connects to TTS, animation, and external communication systems
- **Input**: Response content, delivery specifications
- **Output**: Speech, gestures, animations, external communications

## Architecture Integration

### Eva Integration
The Somatic Triad integrates with the existing Eva chatbot architecture:
- Facial expression control
- Animation coordination
- Speech synthesis
- Gesture management

### Sensory Processing
- **Vision System**: Face detection, object recognition
- **Audio Processing**: Speech recognition, sound analysis
- **Text Processing**: NLP pipeline integration with RelEx and R2L

### Behavioral Execution
- **Action Orchestration**: Coordinates multiple simultaneous actions
- **Conflict Resolution**: Manages competing behavioral requests
- **Timing Coordination**: Ensures proper sequencing of actions

## Communication Patterns

### Internal Communication
- REST APIs for service coordination
- Event-driven updates for real-time behavioral responses
- Shared behavioral state through AtomSpace

### External Communication
- **To Cerebral Triad**: Sensory data, behavioral feedback
- **To Autonomic Triad**: System status, error conditions
- **To External Systems**: Robot control, animation, speech output

## Behavioral Models

The Somatic Triad implements behavioral models based on:
- **OpenPsi**: Goal-driven behavior selection
- **Ghost**: Conversational behavior patterns
- **Eva Model**: Self-awareness and emotional expression

## Deployment

Services are containerized and deployed in Kubernetes with:
- Real-time processing capabilities
- Low-latency communication paths
- Integration with external hardware systems

## Development

Each service includes:
- Behavioral pattern definitions
- Sensory processing pipelines
- Motor control interfaces
- Integration tests with physical systems
