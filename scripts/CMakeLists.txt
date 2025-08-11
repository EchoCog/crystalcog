# Scripts directory - no compilation needed
# This directory contains build and deployment scripts
cmake_minimum_required(VERSION 3.10)

# Install scripts if needed
file(GLOB SCRIPT_FILES "*.sh")
if(SCRIPT_FILES)
    install(FILES ${SCRIPT_FILES}
            DESTINATION ${CMAKE_INSTALL_BINDIR}
            PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
endif()

# Add agent-zero subdirectory if it exists and has CMakeLists.txt
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/agent-zero/CMakeLists.txt")
    add_subdirectory(agent-zero)
endif()