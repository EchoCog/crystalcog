# Blender API - Python scripts for Blender integration
cmake_minimum_required(VERSION 3.10)

project(blender_api)

# Check for Python
find_program(PYTHON_EXECUTABLE python3)

if(PYTHON_EXECUTABLE)
    # Install Python scripts
    file(GLOB PYTHON_FILES "*.py")
    if(PYTHON_FILES)
        install(FILES ${PYTHON_FILES}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/blender_api)
    endif()
    
    # Install scripts directory
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/scripts")
        install(DIRECTORY scripts/
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/blender_api/scripts)
    endif()
    
    # Install rigAPI and rigControl
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/rigAPI")
        install(DIRECTORY rigAPI/
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/blender_api/rigAPI)
    endif()
    
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/rigControl")
        install(DIRECTORY rigControl/
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/blender_api/rigControl)
    endif()
    
    # Install artistic assets
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/artistic")
        install(DIRECTORY artistic/
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/blender_api/artistic)
    endif()
    
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/textures")
        install(DIRECTORY textures/
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/blender_api/textures)
    endif()
else()
    message(STATUS "Python3 not found - Blender API will not be installed")
endif()

# Install Blender files and documentation
file(GLOB BLEND_FILES "*.blend")
if(BLEND_FILES)
    install(FILES ${BLEND_FILES}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/blender_api)
endif()

file(GLOB SHELL_FILES "*.sh")
if(SHELL_FILES)
    install(FILES ${SHELL_FILES}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/blender_api
            PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
endif()

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
    install(FILES README.md README_PROGRESS.md
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/blender_api)
endif()

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/docs")
    install(DIRECTORY docs/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/blender_api)
endif()