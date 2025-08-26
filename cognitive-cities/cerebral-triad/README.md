# Cerebral Triad

## Overview

The Cerebral Triad represents the high-level decision making and coordination component of the Cognitive Cities Architecture. It corresponds to the Cognitive Membrane in the P-System Membrane Architecture and handles core cognitive processing functions.

## Color Code: Yellow

The Cerebral Triad is represented by the color yellow in the neurological metaphor, symbolizing intellectual activity and conscious thought processes.

## Services

### 1. Thought Service
- **Purpose**: Generates intuitive ideas and potential solutions
- **Integration**: Connects to PLN (Probabilistic Logic Networks) for reasoning
- **Input**: Sensory data from Somatic Triad, knowledge from AtomSpace
- **Output**: Generated ideas, hypotheses, and potential solutions

### 2. Processing Director
- **Purpose**: Coordinates processing of information across the triad
- **Integration**: Uses ECAN (Economic Attention Networks) for attention allocation
- **Input**: Processing requests, resource availability
- **Output**: Processing schedules, resource allocations

### 3. Processing Service
- **Purpose**: Executes analytical processing tasks
- **Integration**: Leverages MOSES for pattern learning and optimization
- **Input**: Data to be processed, processing instructions
- **Output**: Processed information, analysis results

### 4. Output Service
- **Purpose**: Formats and delivers processed information
- **Integration**: Connects to Integration Hub for inter-triad communication
- **Input**: Raw processing results, formatting requirements
- **Output**: Formatted responses, decisions, commands

## Architecture Integration

### AtomSpace Integration
All services in the Cerebral Triad read from and write to the central AtomSpace, ensuring consistent knowledge representation across the cognitive system.

### P-System Membrane Rules
The triad operates under P-System membrane rules that govern:
- Information flow between services
- Resource allocation and priority management
- Conflict resolution between competing processes

### Neural-Symbolic Integration
- **PLN Integration**: Probabilistic reasoning for uncertainty handling
- **MOSES Integration**: Evolutionary learning for pattern recognition
- **ECAN Integration**: Attention allocation for cognitive resource management

## Communication Patterns

### Internal Communication
- Services within the triad communicate via REST APIs
- Shared state maintained through AtomSpace
- Event-driven updates for real-time coordination

### External Communication
- **To Somatic Triad**: Behavioral commands and action requests
- **To Autonomic Triad**: Status updates and monitoring data
- **From Integration Hub**: External inputs and system-wide events

## Deployment

The Cerebral Triad services are designed to be deployed as microservices in a Kubernetes cluster, with each service running in its own container and communicating through the cluster network.

## Development

Each service directory contains:
- Service implementation code
- API specifications
- Unit tests
- Integration tests
- Deployment manifests
