# Semantic Vision - Rust project
cmake_minimum_required(VERSION 3.10)

project(semantic-vision)

# Check for Rust/Cargo
find_program(CARGO_EXECUTABLE cargo)

if(CARGO_EXECUTABLE)
    # Build Rust components
    add_custom_target(semantic-vision-rust
        COMMAND ${CARGO_EXECUTABLE} build --release
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Building Rust semantic-vision"
    )
    
    # Install Rust binaries if they exist after build
    install(CODE "
        if(EXISTS \"${CMAKE_CURRENT_SOURCE_DIR}/target/release/semantic-vision\")
            file(INSTALL \"${CMAKE_CURRENT_SOURCE_DIR}/target/release/semantic-vision\"
                 DESTINATION \"${CMAKE_INSTALL_BINDIR}\"
                 USE_SOURCE_PERMISSIONS)
        endif()
    ")
else()
    message(STATUS "Cargo not found - Rust semantic-vision will not be built")
endif()

# Install documentation and data
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
    install(FILES README.md
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/semantic-vision)
endif()

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/figures")
    install(DIRECTORY figures/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/semantic-vision/figures)
endif()