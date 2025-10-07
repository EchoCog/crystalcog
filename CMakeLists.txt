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

# Set CMake policies
cmake_policy(SET CMP0167 NEW)

# Find required packages
find_package(Boost 1.60 COMPONENTS filesystem program_options system thread REQUIRED)
find_package(Python3 COMPONENTS Interpreter Development REQUIRED)
find_package(PkgConfig REQUIRED)

# Include directories
include_directories(${CMAKE_SOURCE_DIR})

# Add cogutil/cmake to module path for custom Find modules
list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cogutil/cmake)

# Add build directories to package search path
list(APPEND CMAKE_PREFIX_PATH ${CMAKE_BINARY_DIR}/cogutil/lib)
list(APPEND CMAKE_PREFIX_PATH ${CMAKE_BINARY_DIR}/atomspace/lib)

# Add subdirectories in dependency order
# 1. Core utilities (cogutil)
if(EXISTS "${CMAKE_SOURCE_DIR}/cogutil/CMakeLists.txt")
    add_subdirectory(cogutil)
else()
    message(WARNING "cogutil not found - skipping")
endif()

# 2. AtomSpace (depends on cogutil)
if(EXISTS "${CMAKE_SOURCE_DIR}/atomspace/CMakeLists.txt")
    add_subdirectory(atomspace)
else()
    message(WARNING "atomspace not found - skipping")
endif()

# 3. Attention Bank (depends on atomspace) - temporarily disabled
# if(EXISTS "${CMAKE_SOURCE_DIR}/attention/CMakeLists.txt")
#     add_subdirectory(attention)
# else()
#     message(WARNING "attention not found - skipping")
# endif()

# 4. URE (depends on atomspace) - temporarily disabled
# if(EXISTS "${CMAKE_SOURCE_DIR}/ure/CMakeLists.txt")
#     add_subdirectory(ure)
# else()
#     message(WARNING "ure not found - skipping")
# endif()

# 5. PLN (depends on atomspace and ure) - temporarily disabled
# if(EXISTS "${CMAKE_SOURCE_DIR}/pln/CMakeLists.txt")
#     add_subdirectory(pln)
# else()
#     message(WARNING "pln not found - skipping")
# endif()

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
if(EXISTS "${CMAKE_SOURCE_DIR}/link-grammar/CMakeLists.txt")
    add_subdirectory(link-grammar)
else()
    message(WARNING "link-grammar not found - skipping")
endif()

# 7. CogServer (depends on atomspace and cogutil)
if(EXISTS "${CMAKE_SOURCE_DIR}/cogserver/CMakeLists.txt")
    add_subdirectory(cogserver)
else()
    message(WARNING "cogserver not found - skipping")
endif()

# 8. Other components that depend on core components
# Use conditional inclusion for all remaining directories to ensure robustness
# Define a helper function for conditional subdirectory inclusion
function(add_subdirectory_if_exists subdir)
    if(EXISTS "${CMAKE_SOURCE_DIR}/${subdir}/CMakeLists.txt")
        add_subdirectory(${subdir})
    else()
        message(STATUS "${subdir} not found or has no CMakeLists.txt - skipping")
    endif()
endfunction()

# Add all remaining subdirectories conditionally
add_subdirectory_if_exists(agents)
add_subdirectory_if_exists(agi-bio)
# add_subdirectory_if_exists(asmoses)  # Disabled - requires URE
add_subdirectory_if_exists(atomspace-agents)
add_subdirectory_if_exists(atomspace-bridge)
add_subdirectory_if_exists(atomspace-cog)
add_subdirectory_if_exists(atomspace-dht)
add_subdirectory_if_exists(atomspace-explorer)
add_subdirectory_if_exists(atomspace-ipfs)
add_subdirectory_if_exists(atomspace-js)
add_subdirectory_if_exists(atomspace-metta)
add_subdirectory_if_exists(atomspace-restful)
add_subdirectory_if_exists(atomspace-rocks)
add_subdirectory_if_exists(atomspace-rpc)
add_subdirectory_if_exists(atomspace-typescript)
add_subdirectory_if_exists(atomspace-websockets)
add_subdirectory_if_exists(benchmark)
add_subdirectory_if_exists(blender_api)
add_subdirectory_if_exists(blender_api_msgs)
add_subdirectory_if_exists(cheminformatics)
add_subdirectory_if_exists(cogprotolab)
add_subdirectory_if_exists(config)
add_subdirectory_if_exists(destin)
add_subdirectory_if_exists(dimensional-embedding)
add_subdirectory_if_exists(distributional-value)
add_subdirectory_if_exists(docker)
add_subdirectory_if_exists(external-tools)
add_subdirectory_if_exists(generate)
add_subdirectory_if_exists(ghost_bridge)
add_subdirectory_if_exists(gnu)
add_subdirectory_if_exists(guile-dbi)
add_subdirectory_if_exists(language-learning)
add_subdirectory_if_exists(learn)
add_subdirectory_if_exists(lg-atomese)
add_subdirectory_if_exists(link-grammar-website)
add_subdirectory_if_exists(linkgrammar-relex-web)
add_subdirectory_if_exists(logicmoo_cogserver)
add_subdirectory_if_exists(loving-ai)
add_subdirectory_if_exists(loving-ai-ghost)
add_subdirectory_if_exists(miner)
add_subdirectory_if_exists(modules)
add_subdirectory_if_exists(moses)
add_subdirectory_if_exists(ocpkg)
add_subdirectory_if_exists(opencog)
add_subdirectory_if_exists(opencog-cycl)
add_subdirectory_if_exists(opencog-debian)
add_subdirectory_if_exists(opencog-neo4j)
add_subdirectory_if_exists(opencog-nix)
add_subdirectory_if_exists(opencog.org)
add_subdirectory_if_exists(opencog_rpi)
add_subdirectory_if_exists(opencog-to-minecraft)
add_subdirectory_if_exists(pattern-index)
add_subdirectory_if_exists(pau2motors)
add_subdirectory_if_exists(perception)
add_subdirectory_if_exists(pi_vision)
add_subdirectory_if_exists(pln-brca-xp)
add_subdirectory_if_exists(prereq)
add_subdirectory_if_exists(profile)
add_subdirectory_if_exists(python-attic)
add_subdirectory_if_exists(python-client)
add_subdirectory_if_exists(python-destin)
add_subdirectory_if_exists(relex)
add_subdirectory_if_exists(rest-api-documentation)
add_subdirectory_if_exists(robots_config)
add_subdirectory_if_exists(rocca)
add_subdirectory_if_exists(ros-behavior-scripting)
add_subdirectory_if_exists(ros_opencog_robot_embodiment)
add_subdirectory_if_exists(scripts)
add_subdirectory_if_exists(semantic-vision)
add_subdirectory_if_exists(sensory)
add_subdirectory_if_exists(spacetime)
add_subdirectory_if_exists(src)
add_subdirectory_if_exists(stochastic-language-generation)
add_subdirectory_if_exists(test-datasets)
add_subdirectory_if_exists(tests)
add_subdirectory_if_exists(TinyCog)
add_subdirectory_if_exists(tv-toolbox)
add_subdirectory_if_exists(unify)
add_subdirectory_if_exists(unity3d-opencog-game)
add_subdirectory_if_exists(vision)
add_subdirectory_if_exists(visualization)

# DrawTerm System - Revolutionary "Infrastructure as Glyphs" Platform
option(ENABLE_DRAWTERM "Enable DrawTerm system for AI workbench deployment" ON)
if(ENABLE_DRAWTERM)
    message(STATUS "DrawTerm system enabled")
    add_subdirectory(drawterm)
endif()

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
