# Guile DBI - Database interface for Guile
cmake_minimum_required(VERSION 3.10)

project(guile-dbi)

# Check if this has existing CMakeLists.txt structure
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt" AND NOT "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt" STREQUAL "${CMAKE_CURRENT_LIST_FILE}")
    # Include existing CMakeLists.txt
    include("${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt")
else()
    # Install Guile DBI files
    file(GLOB_RECURSE GUILE_FILES "*")
    if(GUILE_FILES)
        foreach(guile_file ${GUILE_FILES})
            file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${guile_file})
            get_filename_component(rel_dir ${rel_path} DIRECTORY)
            install(FILES ${guile_file}
                    DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/guile-dbi/${rel_dir})
        endforeach()
    endif()
endif()