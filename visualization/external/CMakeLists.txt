# External dependencies CMakeLists.txt
cmake_minimum_required(VERSION 3.10)

# Add ImGui subdirectory if it exists
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/imgui/imgui.h")
    add_subdirectory(imgui)
endif()