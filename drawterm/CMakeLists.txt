# DrawTerm Implementation CMakeLists.txt
# Revolutionary "Infrastructure as Glyphs" platform

cmake_minimum_required(VERSION 3.10)

# DrawTerm project configuration
project(DrawTerm VERSION 1.0.0 LANGUAGES CXX)

# C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required packages
find_package(Boost 1.60 COMPONENTS filesystem system thread REQUIRED)
find_package(Threads REQUIRED)

# Optional GGML support
option(ENABLE_GGML "Enable GGML AI model support" OFF)

# Include directories
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

# Core DrawTerm libraries
add_subdirectory(src/styx)
add_subdirectory(src/disvm)
add_subdirectory(src/yacc)
add_subdirectory(src/ai)

# Integration library
add_library(drawterm_integration
    src/integration/drawterm_integration.cpp
)

target_link_libraries(drawterm_integration
    styx_protocol
    disvm_engine
    yacc_grammar
    ai_models
    ${Boost_LIBRARIES}
    Threads::Threads
)

target_include_directories(drawterm_integration PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

# Demo executables
add_subdirectory(demos)

# Tests (if testing is enabled)
if(BUILD_TESTING)
    add_subdirectory(tests)
endif()

# Install targets
install(TARGETS drawterm_integration
    EXPORT DrawTermTargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
    RUNTIME DESTINATION bin
    INCLUDES DESTINATION include
)

install(DIRECTORY include/
    DESTINATION include
    FILES_MATCHING PATTERN "*.h" PATTERN "*.hpp"
)

# Export configuration
install(EXPORT DrawTermTargets
    FILE DrawTermTargets.cmake
    NAMESPACE DrawTerm::
    DESTINATION lib/cmake/DrawTerm
)