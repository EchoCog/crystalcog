# Prerequisites directory - no compilation needed
# This directory contains prerequisite installation scripts
cmake_minimum_required(VERSION 3.10)

# Install prerequisite files if they exist
file(GLOB PREREQ_FILES "*")
if(PREREQ_FILES)
    install(FILES ${PREREQ_FILES}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/prereq)
endif()