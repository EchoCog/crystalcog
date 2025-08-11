# RelEx - Relation Extractor
cmake_minimum_required(VERSION 3.10)

project(relex)

# Check if this has existing CMakeLists.txt structure
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt" AND NOT "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt" STREQUAL "${CMAKE_CURRENT_LIST_FILE}")
    # Include existing CMakeLists.txt
    include("${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt")
else()
    # Install RelEx files
    file(GLOB_RECURSE RELEX_FILES "*")
    if(RELEX_FILES)
        foreach(relex_file ${RELEX_FILES})
            file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${relex_file})
            get_filename_component(rel_dir ${rel_path} DIRECTORY)
            install(FILES ${relex_file}
                    DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/relex/${rel_dir})
        endforeach()
    endif()
endif()