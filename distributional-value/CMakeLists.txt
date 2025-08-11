# Distributed Value module
cmake_minimum_required(VERSION 3.10)

project(distributional-value)

# Install distributional value files
file(GLOB DIST_VAL_FILES "*")
if(DIST_VAL_FILES)
    install(FILES ${DIST_VAL_FILES}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/distributional-value)
endif()