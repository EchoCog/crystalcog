# Loving AI - Rust project
cmake_minimum_required(VERSION 3.10)

project(loving-ai)

# Check for Rust/Cargo
find_program(CARGO_EXECUTABLE cargo)

if(CARGO_EXECUTABLE)
    # Build Rust components
    add_custom_target(loving-ai-rust
        COMMAND ${CARGO_EXECUTABLE} build --release
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Building Rust loving-ai"
    )
    
    # Install Rust binaries if they exist after build
    install(CODE "
        if(EXISTS \"${CMAKE_CURRENT_SOURCE_DIR}/target/release/loving-ai\")
            file(INSTALL \"${CMAKE_CURRENT_SOURCE_DIR}/target/release/loving-ai\"
                 DESTINATION \"${CMAKE_INSTALL_BINDIR}\"
                 USE_SOURCE_PERMISSIONS)
        endif()
    ")
else()
    message(STATUS "Cargo not found - Rust loving-ai will not be built")
endif()

# Install all other content files
file(GLOB_RECURSE CONTENT_FILES 
    "*.txt" "*.exp" "*.sh" "*.scm" "*.html" "*.css" "*.js")
if(CONTENT_FILES)
    foreach(content_file ${CONTENT_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${content_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${content_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/loving-ai/${rel_dir})
    endforeach()
endif()

# Install directories with special handling
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/LIVEDATA")
    install(DIRECTORY LIVEDATA/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/loving-ai/LIVEDATA)
endif()

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/ONTOLOGY")
    install(DIRECTORY ONTOLOGY/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/loving-ai/ONTOLOGY)
endif()

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/RAWDATA")
    install(DIRECTORY RAWDATA/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/loving-ai/RAWDATA)
endif()

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/WORLDDATA")
    install(DIRECTORY WORLDDATA/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/loving-ai/WORLDDATA)
endif()

# Install executables with proper permissions
file(GLOB SCRIPT_FILES "*.sh")
if(SCRIPT_FILES)
    install(FILES ${SCRIPT_FILES}
            DESTINATION ${CMAKE_INSTALL_BINDIR}
            PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
endif()