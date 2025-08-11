# OpenCog Cycl integration
cmake_minimum_required(VERSION 3.10)

project(opencog-cycl)

# Install Cycl integration files
file(GLOB CYCL_FILES "*")
if(CYCL_FILES)
    install(FILES ${CYCL_FILES}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/opencog-cycl)
endif()