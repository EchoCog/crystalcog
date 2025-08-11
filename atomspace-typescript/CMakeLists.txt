# AtomSpace TypeScript/React visualizer
cmake_minimum_required(VERSION 3.10)

project(atomspace-typescript)

# Check for Node.js and npm
find_program(NODE_EXECUTABLE node)
find_program(NPM_EXECUTABLE npm)

if(NODE_EXECUTABLE AND NPM_EXECUTABLE)
    # Build TypeScript/React application
    add_custom_target(atomspace-typescript-build
        COMMAND ${NPM_EXECUTABLE} install
        COMMAND ${NPM_EXECUTABLE} run build
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Building AtomSpace TypeScript visualizer"
    )
    
    # Install built application
    install(CODE "
        if(EXISTS \"${CMAKE_CURRENT_SOURCE_DIR}/build\")
            file(INSTALL \"${CMAKE_CURRENT_SOURCE_DIR}/build/\"
                 DESTINATION \"${CMAKE_INSTALL_DATADIR}/opencog/atomspace-typescript\"
                 USE_SOURCE_PERMISSIONS)
        endif()
    ")
else()
    message(STATUS "Node.js/npm not found - TypeScript visualizer will not be built")
endif()

# Install source files for development
install(FILES package.json tsconfig.json
        DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/atomspace-typescript/src)

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/src")
    install(DIRECTORY src/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/atomspace-typescript/src/src)
endif()

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/public")
    install(DIRECTORY public/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/atomspace-typescript/src/public)
endif()

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
    install(FILES README.md
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/atomspace-typescript)
endif()