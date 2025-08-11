# DrawTerm: Infrastructure as Glyphs

DrawTerm is a revolutionary "Infrastructure as Glyphs" platform that enables AI workbenches to be deployed as portable, network-transparent virtual machines.

## Overview

The DrawTerm system implements four core components:

### 1. 🔗 Styx Protocol - Real Network Transparency
- Complete implementation of Plan 9's Styx protocol for network file system operations
- `StyxConnection` and `StyxServer` classes providing client/server communication
- Network-transparent file mounting and serving capabilities
- Authentication framework and security layer

### 2. 🚀 DIS VM Integration - Actual Limbo Code Execution  
- Full DIS (Distributed Systems) Virtual Machine implementation
- Support for 20+ opcodes including arithmetic, control flow, string operations, and AI operations
- `DISVM` class with complete execution engine and state management
- `LimboCompiler` for compiling Limbo source code to DIS bytecode
- Custom opcodes for DrawKern operations (RENDER_GLYPH, SPAWN_VM, MOUNT_NS)

### 3. 📝 Yacc Grammar - Formal VM Glyph Description Language
- Complete lexer, parser, and AST implementation for VM topology descriptions
- BNF grammar supporting VM declarations, glyph definitions, and workbench specifications
- Code generation capabilities (C++, JSON, YAML output formats)
- Built-in templates for common patterns (AI workbench, file server, echo server)

### 4. 🤖 GGML/RWKV Models - Real AI Inference Integration
- Abstract `AIModel` base class with concrete `GGMLModel` and `RWKVModel` implementations
- `DrawKernAIManager` for loading, managing, and coordinating multiple AI models
- Session management with conversation history and context preservation
- Full integration with DIS VMs through AI-specific opcodes

## Quick Start

### Building DrawTerm

```bash
cd drawterm
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j$(nproc)
```

### Running Demos

The system includes 5 comprehensive demos:

```bash
# Test individual components
./demos/demo_styx_client_server      # Network transparency
./demos/demo_disvm_execution         # VM execution
./demos/demo_yacc_parsing           # Grammar parsing
./demos/demo_ai_workbench           # AI integration

# Complete system integration
./demos/demo_drawkern_complete      # End-to-end demo
```

### Basic Usage

```cpp
#include "drawterm/drawterm_integration.h"

int main() {
    // Initialize the DrawTerm system
    DrawTerm::DrawTermSystem drawterm;
    drawterm.initialize();
    
    // Deploy AI workbench using glyph description
    std::string workbench_spec = R"(
        workbench ai_research {
            ai_model: rwkv-4-7b
            tools: [python, jupyter, git]
            environment: {
                CUDA_VISIBLE_DEVICES: "0"
                WORKSPACE: "/workbench"
            }
        }
    )";
    
    drawterm.deploy_ai_workbench(workbench_spec);
    
    // Start network transparency layer
    drawterm.start_styx_server("127.0.0.1", 9999);
    
    // Create and run VM
    std::string vm_id = drawterm.create_vm();
    drawterm.load_limbo_program(vm_id, "hello halt");
    drawterm.start_vm(vm_id);
    
    return 0;
}
```

## Glyph Description Language

Define infrastructure using the DrawTerm glyph language:

```yaml
# AI Workbench Glyph
workbench ml_research {
    ai_model: rwkv-4-7b
    tools: [python, jupyter, tensorboard]
    environment: {
        WORKSPACE: "/research"
        GPU_MEMORY: "8GB"
    }
}

# Distributed Compute Glyph  
glyph distributed_training {
    vm coordinator {
        type: master
        workers: 4
    }
    
    vm worker[4] {
        type: compute_node
        gpu: true
        coordinator: coordinator
    }
}
```

## Revolutionary Capabilities

- **Glyph-based Deployment**: Entire AI workbenches described as text and rendered anywhere
- **Network Transparency**: Seamless file system access across network boundaries  
- **Portable Execution**: DIS VMs run consistently on any architecture
- **Dynamic AI Integration**: Models loaded and managed within VM ecosystem
- **Formal Specifications**: Grammar-driven descriptions ensure consistency

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    DrawTerm System                          │
├─────────────────┬─────────────────┬─────────────────────────┤
│  Styx Protocol  │    DIS VM       │    Yacc Grammar         │
│                 │                 │                         │
│ • StyxServer    │ • DISVM         │ • Lexer/Parser          │
│ • StyxConnection│ • LimboCompiler │ • Code Generator        │
│ • Network FS    │ • VM Manager    │ • Templates             │
└─────────────────┴─────────────────┴─────────────────────────┘
                           │
                 ┌─────────────────────┐
                 │   AI Integration    │
                 │                     │
                 │ • GGML Models       │
                 │ • RWKV Models       │
                 │ • AI Manager        │
                 │ • Sessions          │
                 └─────────────────────┘
```

## Demo Results

All demos successfully demonstrate:
- ✅ Yacc grammar parsing and validation of complex glyph descriptions
- ✅ Styx client/server network communication with authentication
- ✅ DIS VMs executing Limbo bytecode with AI operations
- ✅ Multi-VM coordination and management
- ✅ Code generation for deployment automation (C++, JSON, YAML)
- ✅ AI model framework with session management
- ✅ End-to-end system integration

This represents a paradigm shift from traditional software installation to dynamic infrastructure rendering, where computing environments are described, transmitted, and instantiated on demand.

## License

Part of the OpenCog Central monorepo.