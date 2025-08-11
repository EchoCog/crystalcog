# Source directory - main source code
cmake_minimum_required(VERSION 3.10)

# Add agent-zero subdirectory if it exists and has CMakeLists.txt
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/agent-zero/CMakeLists.txt")
    add_subdirectory(agent-zero)
endif()