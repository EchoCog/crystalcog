# AtomSpace Explorer
cmake_minimum_required(VERSION 3.10)

project(atomspace-explorer)

# Check for Node.js and npm for web interface
find_program(NODE_EXECUTABLE node)
find_program(NPM_EXECUTABLE npm)

if(NODE_EXECUTABLE AND NPM_EXECUTABLE AND EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/package.json")
    # Build web interface
    add_custom_target(atomspace-explorer-build
        COMMAND ${NPM_EXECUTABLE} install
        COMMAND ${NPM_EXECUTABLE} run build
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Building AtomSpace Explorer web interface"
    )
    
    # Install built application
    install(CODE "
        if(EXISTS \"${CMAKE_CURRENT_SOURCE_DIR}/build\")
            file(INSTALL \"${CMAKE_CURRENT_SOURCE_DIR}/build/\"
                 DESTINATION \"${CMAKE_INSTALL_DATADIR}/opencog/atomspace-explorer\"
                 USE_SOURCE_PERMISSIONS)
        endif()
    ")
else()
    message(STATUS "Node.js/npm not found - AtomSpace Explorer web interface will not be built")
endif()

# Install source files and documentation
file(GLOB_RECURSE EXPLORER_FILES "*.md" "*.json" "*.js" "*.ts" "*.html" "*.css")
if(EXPLORER_FILES)
    foreach(explorer_file ${EXPLORER_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${explorer_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${explorer_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/atomspace-explorer/src/${rel_dir})
    endforeach()
endif()