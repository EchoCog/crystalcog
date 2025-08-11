# Tests directory - integration tests
cmake_minimum_required(VERSION 3.10)

# Add agent-zero tests if they exist
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/agent-zero/CMakeLists.txt")
    add_subdirectory(agent-zero)
endif()

# Install test scripts if needed
file(GLOB TEST_SCRIPTS "*.sh" "*.py")
if(TEST_SCRIPTS)
    install(FILES ${TEST_SCRIPTS}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/tests
            PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
endif()