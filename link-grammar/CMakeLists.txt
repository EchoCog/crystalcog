# Link Grammar - natural language parser
cmake_minimum_required(VERSION 3.10)

project(link-grammar)

# Check if this has existing CMakeLists.txt structure
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt" AND NOT "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt" STREQUAL "${CMAKE_CURRENT_LIST_FILE}")
    # Include existing CMakeLists.txt
    include("${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt")
else()
    # Install Link Grammar files
    file(GLOB_RECURSE LG_FILES "*")
    if(LG_FILES)
        foreach(lg_file ${LG_FILES})
            file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${lg_file})
            get_filename_component(rel_dir ${rel_path} DIRECTORY)
            install(FILES ${lg_file}
                    DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/link-grammar/${rel_dir})
        endforeach()
    endif()
endif()