# Configuration directory - no compilation needed
# This directory contains configuration files
cmake_minimum_required(VERSION 3.10)

# Install configuration files
file(GLOB CONFIG_FILES "*.scm" "*.conf" "*.cfg" "*.json")
if(CONFIG_FILES)
    install(FILES ${CONFIG_FILES}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/config)
endif()