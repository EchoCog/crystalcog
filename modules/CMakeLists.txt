# Modules directory
cmake_minimum_required(VERSION 3.10)

# Install module files
file(GLOB MODULE_FILES "*")
if(MODULE_FILES)
    install(FILES ${MODULE_FILES}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/modules)
endif()