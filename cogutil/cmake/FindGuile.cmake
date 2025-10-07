# FindGuile.cmake
# Find Guile using pkg-config

# Use pkg-config to find Guile
find_package(PkgConfig REQUIRED)
pkg_check_modules(GUILE guile-3.0)

if(GUILE_FOUND)
    set(GUILE_INCLUDE_DIR ${GUILE_INCLUDE_DIRS})
    set(GUILE_LIBRARIES ${GUILE_LIBRARIES})
    set(GUILE_LIBRARY_DIRS ${GUILE_LIBRARY_DIRS})
    set(GUILE_LDFLAGS ${GUILE_LDFLAGS})
    set(GUILE_CFLAGS ${GUILE_CFLAGS})
    set(GUILE_CFLAGS_OTHER ${GUILE_CFLAGS_OTHER})
    
    # Find the Guile executable
    find_program(GUILE_EXECUTABLE guile-3.0)
    if(NOT GUILE_EXECUTABLE)
        find_program(GUILE_EXECUTABLE guile)
    endif()
    
    # Find guile-config
    find_program(GUILE_CONFIG_EXECUTABLE guile-config-3.0)
    if(NOT GUILE_CONFIG_EXECUTABLE)
        find_program(GUILE_CONFIG_EXECUTABLE guile-config)
    endif()
    
    # Find guild
    find_program(GUILD_EXECUTABLE guild-3.0)
    if(NOT GUILD_EXECUTABLE)
        find_program(GUILD_EXECUTABLE guild)
    endif()
    
    set(GUILE_VERSION ${GUILE_VERSION})
    message(STATUS "Found Guile: ${GUILE_VERSION}")
    message(STATUS "  Include dirs: ${GUILE_INCLUDE_DIRS}")
    message(STATUS "  Libraries: ${GUILE_LIBRARIES}")
    message(STATUS "  Executable: ${GUILE_EXECUTABLE}")
    message(STATUS "  Config: ${GUILE_CONFIG_EXECUTABLE}")
    message(STATUS "  Guild: ${GUILD_EXECUTABLE}")
else()
    set(GUILE_FOUND FALSE)
    message(STATUS "Guile not found")
endif()

# Handle the QUIETLY and REQUIRED arguments
include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Guile
    REQUIRED_VARS GUILE_LIBRARIES GUILE_INCLUDE_DIR
    VERSION_VAR GUILE_VERSION
)