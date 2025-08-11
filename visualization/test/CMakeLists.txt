# Simple test CMakeLists.txt for ImGui visualizer
cmake_minimum_required(VERSION 3.10)
project(imgui-test)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required packages
find_package(OpenGL REQUIRED)
find_package(glfw3 REQUIRED)
find_package(Boost COMPONENTS system REQUIRED)

# Include directories
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/../external/imgui
    ${CMAKE_CURRENT_SOURCE_DIR}/../external/imgui/backends
)

# ImGui sources
set(IMGUI_SOURCES
    ../external/imgui/imgui.cpp
    ../external/imgui/imgui_demo.cpp
    ../external/imgui/imgui_draw.cpp
    ../external/imgui/imgui_tables.cpp
    ../external/imgui/imgui_widgets.cpp
    ../external/imgui/backends/imgui_impl_glfw.cpp
    ../external/imgui/backends/imgui_impl_opengl3.cpp
)

# Test application sources
set(APP_SOURCES
    main_test.cpp
)

# Create executable
add_executable(imgui_test ${IMGUI_SOURCES} ${APP_SOURCES})

# Link libraries
target_link_libraries(imgui_test
    OpenGL::GL
    glfw
    ${Boost_SYSTEM_LIBRARY}
)

# Compile definitions
target_compile_definitions(imgui_test PRIVATE
    IMGUI_IMPL_OPENGL_LOADER_GLAD=0
)