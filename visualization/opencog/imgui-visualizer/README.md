# OpenCog ImGui Visualizer

This directory contains the ImGui-based visualization system for OpenCog AtomSpace. ImGui provides a modern, immediate-mode graphical user interface that allows for real-time exploration and manipulation of AtomSpace data.

## Features

- **Interactive AtomSpace Visualization**: Browse and explore atoms in real-time
- **Graph Visualization**: Visual representation of atom relationships and connections
- **CogServer Integration**: Connect to running CogServer instances to access live AtomSpace data
- **Search and Filtering**: Find atoms by name, type, UUID, or other properties
- **Truth Value Display**: Visualize strength, confidence, and count values
- **Modern UI**: Responsive interface with docking, viewports, and customizable layouts

## Requirements

- OpenGL 3.0+ compatible graphics
- GLFW 3.x
- Boost libraries (system component)
- C++17 compiler

## Building

The ImGui visualizer is integrated into the OpenCog visualization build system:

```bash
cd opencog-central/visualization
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make
```

If ImGui is detected in the `external/imgui` directory, the visualizer will be built automatically.

## Running

### Standalone Test
```bash
cd visualization/test/build
./imgui_test
```

### With OpenCog Integration
```bash
cd visualization/build
./OpenCogImGuiVisualizer
```

## Usage

1. **Connection Setup**: Configure the CogServer address (default: localhost:17001)
2. **Search**: Use the search panel to find atoms by various criteria
3. **Explore**: Select atoms from the results to view details and connections
4. **Visualize**: Use the graph view to see relationships between atoms
5. **Interact**: Zoom, pan, and click on nodes in the graph visualization

## Configuration

### Connection Settings
- Server address: Configure the CogServer host and port
- Auto-refresh: Enable automatic data updates
- Refresh interval: Set how often to update data (1-30 seconds)

### Display Options  
- Search panel: Toggle search interface visibility
- Atom details: Show/hide detailed atom information
- Graph view: Enable/disable graph visualization
- Node radius: Adjust visual size of graph nodes
- Link thickness: Configure connection line width

## Integration with OpenCog Components

The ImGui visualizer integrates with existing OpenCog components:

- **AtomSpace**: Uses the same AtomSpaceInterface as the GTK visualizer
- **CogServer**: Connects via TCP to retrieve atom data
- **AtomTypes**: Leverages existing atom type system for classification
- **Truth Values**: Displays and manipulates truth value data

## Architecture

```
ImGui Visualizer
├── main.cpp                     # Application entry point and main loop
├── AtomSpaceImGuiInterface.h/cpp # OpenCog integration wrapper
├── ImGuiAtomGraph.h/cpp         # Graph visualization component  
└── CMakeLists.txt               # Build configuration
```

### Key Classes

- **AtomSpaceImGuiInterface**: Main interface between ImGui and OpenCog components
- **ImGuiAtomGraph**: Interactive graph visualization with zoom/pan/selection
- **AtomSpaceInterface**: Reused from GTK visualizer for CogServer communication

## Extension Points

The visualizer can be extended with additional features:

- Custom atom rendering based on types
- Advanced graph layout algorithms  
- Truth value editing capabilities
- Export functionality (graphs, data)
- Plugin system for custom visualizations

## Comparison with GTK Visualizer

| Feature | GTK Visualizer | ImGui Visualizer |
|---------|----------------|------------------|
| UI Framework | GTK3 | ImGui (OpenGL) |
| Performance | Good | Excellent |
| Modern UI | Basic | Advanced |
| Docking | No | Yes |
| Multi-viewport | No | Yes |
| Graph Interaction | Limited | Full (zoom/pan/select) |
| Customization | Limited | Highly customizable |

## Troubleshooting

### Build Issues
- Ensure OpenGL development libraries are installed
- Verify GLFW3 is available: `pkg-config --libs glfw3`
- Check Boost libraries: `find /usr -name "*boost*system*"`

### Runtime Issues  
- OpenGL context errors: Update graphics drivers
- Connection failures: Verify CogServer is running and accessible
- Performance issues: Adjust refresh intervals and limit search results

## Contributing

When contributing to the ImGui visualizer:

1. Follow existing code style and patterns
2. Maintain compatibility with the AtomSpaceInterface
3. Test with both real and mock AtomSpace data
4. Document new features and configuration options
5. Consider performance impact of UI updates

## Future Enhancements

- 3D atom graph visualization
- Collaborative editing capabilities
- Advanced filtering and search options
- Integration with other OpenCog tools
- Mobile/web-based interfaces using ImGui backends