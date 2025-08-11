# Agents directory - Rust project with Scheme modules
cmake_minimum_required(VERSION 3.10)

project(agents)

# Check for Rust/Cargo
find_program(CARGO_EXECUTABLE cargo)

if(CARGO_EXECUTABLE)
    # Build Rust components
    add_custom_target(agents-rust
        COMMAND ${CARGO_EXECUTABLE} build --release
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Building Rust agents"
    )
    
    # Install Rust binaries if they exist after build
    install(CODE "
        if(EXISTS \"${CMAKE_CURRENT_SOURCE_DIR}/target/release/agents\")
            file(INSTALL \"${CMAKE_CURRENT_SOURCE_DIR}/target/release/agents\"
                 DESTINATION \"${CMAKE_INSTALL_BINDIR}\"
                 USE_SOURCE_PERMISSIONS)
        endif()
    ")
else()
    message(STATUS "Cargo not found - Rust agents will not be built")
endif()

# Install Scheme modules
file(GLOB_RECURSE SCM_FILES "*.scm")
if(SCM_FILES)
    foreach(scm_file ${SCM_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${scm_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${scm_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/scm/agents/${rel_dir})
    endforeach()
endif()

# Install examples and documentation
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/examples")
    install(DIRECTORY examples/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/examples/agents)
endif()

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
    install(FILES README.md
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/agents)
endif()