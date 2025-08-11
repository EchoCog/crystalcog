# Styx Protocol Implementation
cmake_minimum_required(VERSION 3.10)

# Create the Styx protocol library
add_library(styx_protocol
    styx_connection.cpp
    styx_server.cpp
    styx_message.cpp
    styx_auth.cpp
)

target_include_directories(styx_protocol PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../include>
    $<INSTALL_INTERFACE:include>
    ${Boost_INCLUDE_DIRS}
)

target_link_libraries(styx_protocol
    ${Boost_LIBRARIES}
    Threads::Threads
)

# Set library properties
set_target_properties(styx_protocol PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED ON
)

# Install target
install(TARGETS styx_protocol
    EXPORT DrawTermTargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)