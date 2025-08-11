# GNU directory - configuration and scripts
cmake_minimum_required(VERSION 3.10)

# Install GNU-related files
file(GLOB GNU_FILES "*")
if(GNU_FILES)
    install(FILES ${GNU_FILES}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/gnu)
endif()