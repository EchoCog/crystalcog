# ROCCA - Rust project  
cmake_minimum_required(VERSION 3.10)

project(rocca)

# Check for Rust/Cargo
find_program(CARGO_EXECUTABLE cargo)

if(CARGO_EXECUTABLE)
    # Build Rust components
    add_custom_target(rocca-rust
        COMMAND ${CARGO_EXECUTABLE} build --release
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Building Rust ROCCA"
    )
    
    # Install Rust binaries if they exist after build
    install(CODE "
        if(EXISTS \"${CMAKE_CURRENT_SOURCE_DIR}/target/release/rocca\")
            file(INSTALL \"${CMAKE_CURRENT_SOURCE_DIR}/target/release/rocca\"
                 DESTINATION \"${CMAKE_INSTALL_BINDIR}\"
                 USE_SOURCE_PERMISSIONS)
        endif()
    ")
else()
    message(STATUS "Cargo not found - Rust ROCCA will not be built")
endif()

# Check for Python components
find_program(PYTHON_EXECUTABLE python3)
if(PYTHON_EXECUTABLE AND EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/setup.py")
    # Install Python package
    add_custom_target(rocca-python
        COMMAND ${PYTHON_EXECUTABLE} setup.py build
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Building Python ROCCA components"
    )
    
    install(CODE "
        execute_process(
            COMMAND ${PYTHON_EXECUTABLE} setup.py install --prefix=${CMAKE_INSTALL_PREFIX}
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        )
    ")
endif()

# Install documentation and examples
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
    install(FILES README.md
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/rocca)
endif()

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/examples")
    install(DIRECTORY examples/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/examples/rocca)
endif()

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/doc")
    install(DIRECTORY doc/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/rocca)
endif()