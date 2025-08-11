# Profile directory - no compilation needed
# This directory contains profiling configuration
cmake_minimum_required(VERSION 3.10)

# Install profile files if they exist
file(GLOB PROFILE_FILES "*")
if(PROFILE_FILES)
    install(FILES ${PROFILE_FILES}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/profile)
endif()