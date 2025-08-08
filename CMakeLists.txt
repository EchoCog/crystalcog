cmake_minimum_required(VERSION 3.10)

project(opencog-central)

# Set default build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Global settings
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Enable ccache if available
find_program(CCACHE_PROGRAM ccache)
if(CCACHE_PROGRAM)
    set(CMAKE_C_COMPILER_LAUNCHER ${CCACHE_PROGRAM})
    set(CMAKE_CXX_COMPILER_LAUNCHER ${CCACHE_PROGRAM})
endif()

# Set output directories
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

# Find required packages
find_package(Boost 1.60 COMPONENTS filesystem program_options system thread REQUIRED)
find_package(Python3 COMPONENTS Interpreter Development REQUIRED)
find_package(PkgConfig REQUIRED)

# Include directories
include_directories(${CMAKE_SOURCE_DIR})

# Add subdirectories in dependency order
# 1. Core utilities (cogutil)
add_subdirectory(cogutil)

# 2. AtomSpace (depends on cogutil)
add_subdirectory(atomspace)

# 3. Attention Bank (depends on atomspace)
add_subdirectory(attention)

# 4. URE (depends on atomspace)
add_subdirectory(ure)

# 5. PLN (depends on atomspace and ure)
add_subdirectory(pln)

# Agent-Zero Genesis Integration (optional)
option(ENABLE_AGENT_ZERO "Enable Agent-Zero Genesis cognitive orchestration" ON)
if(ENABLE_AGENT_ZERO)
    message(STATUS "Agent-Zero Genesis integration enabled")
    
    # Find Guile for Agent-Zero modules
    find_program(GUILE_EXECUTABLE guile)
    find_program(GUILD_EXECUTABLE guild)
    
    if(GUILE_EXECUTABLE AND GUILD_EXECUTABLE)
        message(STATUS "Found Guile: ${GUILE_EXECUTABLE}")
        message(STATUS "Found Guild: ${GUILD_EXECUTABLE}")
        
        # Add Agent-Zero C components
        add_subdirectory(src/agent-zero EXCLUDE_FROM_ALL)
        
        # Add custom targets for Agent-Zero
        add_custom_target(agent-zero-modules
            COMMAND ${CMAKE_SOURCE_DIR}/scripts/agent-zero/build-agent-zero.sh --guile-only
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
            COMMENT "Building Agent-Zero Guile modules"
        )
        
        add_custom_target(agent-zero-test
            COMMAND ${CMAKE_SOURCE_DIR}/tests/agent-zero/integration-test.sh
            WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
            DEPENDS agent-zero-modules
            COMMENT "Running Agent-Zero integration tests"
        )
        
        # Add to main build targets
        add_dependencies(all-components agent-zero-modules)
        
    else()
        message(WARNING "Guile not found - Agent-Zero modules will not be built")
    endif()
endif()

# 6. Link Grammar (depends on atomspace)
add_subdirectory(link-grammar)

# 7. CogServer (depends on atomspace and cogutil)
add_subdirectory(cogserver)

# 8. Other components that depend on core components
add_subdirectory(asmoses)
add_subdirectory(agents)
add_subdirectory(agi-bio)
add_subdirectory(atomspace-agents)
add_subdirectory(atomspace-bridge)
add_subdirectory(atomspace-cog)
add_subdirectory(atomspace-dht)
add_subdirectory(atomspace-explorer)
add_subdirectory(atomspace-ipfs)
add_subdirectory(atomspace-js)
add_subdirectory(atomspace-metta)
add_subdirectory(atomspace-restful)
add_subdirectory(atomspace-rocks)
add_subdirectory(atomspace-rpc)
add_subdirectory(atomspace-typescript)
add_subdirectory(atomspace-websockets)
add_subdirectory(benchmark)
add_subdirectory(blender_api)
add_subdirectory(blender_api_msgs)
add_subdirectory(cheminformatics)
add_subdirectory(cogprotolab)
add_subdirectory(destin)
add_subdirectory(dimensional-embedding)
add_subdirectory(distributional-value)
add_subdirectory(external-tools)
add_subdirectory(generate)
add_subdirectory(ghost_bridge)
add_subdirectory(guile-dbi)
add_subdirectory(language-learning)
add_subdirectory(learn)
add_subdirectory(lg-atomese)
add_subdirectory(logicmoo_cogserver)
add_subdirectory(loving-ai)
add_subdirectory(loving-ai-ghost)
add_subdirectory(miner)
add_subdirectory(moses)
add_subdirectory(ocpkg)
add_subdirectory(opencog)
add_subdirectory(opencog-cycl)
add_subdirectory(opencog-debian)
add_subdirectory(opencog-neo4j)
add_subdirectory(opencog-nix)
add_subdirectory(opencog.org)
add_subdirectory(opencog_rpi)
add_subdirectory(opencog-to-minecraft)
add_subdirectory(pattern-index)
add_subdirectory(pau2motors)
add_subdirectory(perception)
add_subdirectory(pi_vision)
add_subdirectory(pln-brca-xp)
add_subdirectory(python-attic)
add_subdirectory(python-client)
add_subdirectory(python-destin)
add_subdirectory(relex)
add_subdirectory(rest-api-documentation)
add_subdirectory(robots_config)
add_subdirectory(rocca)
add_subdirectory(ros-behavior-scripting)
add_subdirectory(ros_opencog_robot_embodiment)
add_subdirectory(semantic-vision)
add_subdirectory(sensory)
add_subdirectory(spacetime)
add_subdirectory(stochastic-language-generation)
add_subdirectory(test-datasets)
add_subdirectory(TinyCog)
add_subdirectory(tv-toolbox)
add_subdirectory(unify)
add_subdirectory(unity3d-opencog-game)
add_subdirectory(vision)
add_subdirectory(visualization)

# Install configuration
include(GNUInstallDirs)

# Create a custom target for building all components
add_custom_target(all-components
    DEPENDS cogutil atomspace attention ure pln link-grammar cogserver
    COMMENT "Building all OpenCog components"
)

# Create a custom target for testing all components
add_custom_target(test-all
    DEPENDS cogutil atomspace attention ure pln link-grammar cogserver
    COMMAND ${CMAKE_CTEST_COMMAND} --output-on-failure
    COMMENT "Running all tests"
)

# Create a custom target for installing all components
add_custom_target(install-all
    DEPENDS all-components
    COMMAND ${CMAKE_COMMAND} --build ${CMAKE_BINARY_DIR} --target install
    COMMENT "Installing all components"
)
