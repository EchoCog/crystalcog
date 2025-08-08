# Agent-Zero C Components CMakeLists.txt
# /src/agent-zero/CMakeLists.txt

cmake_minimum_required(VERSION 3.10)

# Agent-Zero C library
project(agent-zero-cognitive)

set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 17)

# Include directories
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

# Agent-Zero source files
set(AGENT_ZERO_SOURCES
    cognitive-tensors.c
    opencog-ggml-bridge.c
)

# Create shared library
add_library(agent-zero-cognitive SHARED ${AGENT_ZERO_SOURCES})

# Set library properties
set_target_properties(agent-zero-cognitive PROPERTIES
    VERSION 1.0.0
    SOVERSION 1
    PUBLIC_HEADER cognitive.h
)

# Installation targets
install(TARGETS agent-zero-cognitive
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/agent-zero
)

# Optional: Create static library as well
add_library(agent-zero-cognitive-static STATIC ${AGENT_ZERO_SOURCES})
set_target_properties(agent-zero-cognitive-static PROPERTIES
    OUTPUT_NAME agent-zero-cognitive
)

install(TARGETS agent-zero-cognitive-static
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)

# Add to parent build if called from parent
if(NOT CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    # We're being built as part of the main project
    set_property(GLOBAL APPEND PROPERTY AGENT_ZERO_TARGETS agent-zero-cognitive)
endif()

# Testing
if(BUILD_TESTING)
    enable_testing()
    
    # Simple C test
    add_executable(test-agent-zero-c test-cognitive.c)
    target_link_libraries(test-agent-zero-c agent-zero-cognitive)
    add_test(NAME agent-zero-c-test COMMAND test-agent-zero-c)
endif()

# Documentation
if(DOXYGEN_FOUND)
    set(DOXYGEN_PROJECT_NAME "Agent-Zero Cognitive Library")
    set(DOXYGEN_PROJECT_BRIEF "Cognitive tensor operations and OpenCog bridge")
    doxygen_add_docs(agent-zero-docs
        ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Generate Agent-Zero documentation"
    )
endif()