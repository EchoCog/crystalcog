# DIS VM Implementation
cmake_minimum_required(VERSION 3.10)

# Create the DIS VM library
add_library(disvm_engine
    disvm.cpp
    limbo_compiler.cpp
    disvm_manager.cpp
    instruction.cpp
    program.cpp
)

target_include_directories(disvm_engine PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../include>
    $<INSTALL_INTERFACE:include>
    ${Boost_INCLUDE_DIRS}
)

target_link_libraries(disvm_engine
    ${Boost_LIBRARIES}
    Threads::Threads
)

# Set library properties
set_target_properties(disvm_engine PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED ON
)

# Install target
install(TARGETS disvm_engine
    EXPORT DrawTermTargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)