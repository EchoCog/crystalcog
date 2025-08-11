#
# ImGui Visualizer CMakeLists.txt
# Integration with OpenCog AtomSpace via existing AtomSpaceInterface
#

# Find required packages
find_package(OpenGL REQUIRED)
find_package(glfw3 REQUIRED)

# Check if ImGui is available
if(EXISTS "${CMAKE_SOURCE_DIR}/external/imgui/imgui.h")
    message(STATUS "ImGui found - ImGui visualizer will be built")
    
    # Add ImGui subdirectory
    add_subdirectory(${CMAKE_SOURCE_DIR}/external/imgui imgui)
    
    # Create the ImGui visualizer executable
    add_executable(OpenCogImGuiVisualizer
        main.cpp
        AtomSpaceImGuiInterface.cpp
        ImGuiAtomGraph.cpp
    )
    
    # Include directories
    target_include_directories(OpenCogImGuiVisualizer PRIVATE
        ${CMAKE_SOURCE_DIR}/external/imgui
        ${CMAKE_SOURCE_DIR}/external/imgui/backends
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_SOURCE_DIR}/gtk-visualizer  # Reuse existing AtomSpaceInterface
    )
    
    # Link libraries
    target_link_libraries(OpenCogImGuiVisualizer
        imgui
        OpenGL::GL
        glfw
        ${Boost_SYSTEM_LIBRARY}
    )
    
    # Copy existing AtomSpace interface files for now
    # This allows us to reuse the existing CogServer connectivity
    configure_file(
        ${CMAKE_SOURCE_DIR}/gtk-visualizer/AtomSpaceInterface.h
        ${CMAKE_CURRENT_BINARY_DIR}/AtomSpaceInterface.h
        COPYONLY
    )
    configure_file(
        ${CMAKE_SOURCE_DIR}/gtk-visualizer/AtomSpaceInterface.cpp
        ${CMAKE_CURRENT_BINARY_DIR}/AtomSpaceInterface.cpp
        COPYONLY
    )
    configure_file(
        ${CMAKE_SOURCE_DIR}/gtk-visualizer/AtomTypes.h
        ${CMAKE_CURRENT_BINARY_DIR}/AtomTypes.h
        COPYONLY
    )
    configure_file(
        ${CMAKE_SOURCE_DIR}/gtk-visualizer/AtomTypes.cpp
        ${CMAKE_CURRENT_BINARY_DIR}/AtomTypes.cpp
        COPYONLY
    )
    configure_file(
        ${CMAKE_SOURCE_DIR}/gtk-visualizer/Vertex.h
        ${CMAKE_CURRENT_BINARY_DIR}/Vertex.h
        COPYONLY
    )
    configure_file(
        ${CMAKE_SOURCE_DIR}/gtk-visualizer/Vertex.cpp
        ${CMAKE_CURRENT_BINARY_DIR}/Vertex.cpp
        COPYONLY
    )
    
    # Add copied files to build
    target_sources(OpenCogImGuiVisualizer PRIVATE
        ${CMAKE_CURRENT_BINARY_DIR}/AtomSpaceInterface.cpp
        ${CMAKE_CURRENT_BINARY_DIR}/AtomTypes.cpp
        ${CMAKE_CURRENT_BINARY_DIR}/Vertex.cpp
    )
    
else()
    message(STATUS "ImGui not found - ImGui visualizer will not be built")
endif()