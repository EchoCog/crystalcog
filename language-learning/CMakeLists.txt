# Language Learning
cmake_minimum_required(VERSION 3.10)

project(language-learning)

# Check if this has existing CMakeLists.txt structure
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt" AND NOT "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt" STREQUAL "${CMAKE_CURRENT_LIST_FILE}")
    # Include existing CMakeLists.txt
    include("${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt")
else()
    # Install language learning files
    file(GLOB_RECURSE LL_FILES "*")
    if(LL_FILES)
        foreach(ll_file ${LL_FILES})
            file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${ll_file})
            get_filename_component(rel_dir ${rel_path} DIRECTORY)
            install(FILES ${ll_file}
                    DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/language-learning/${rel_dir})
        endforeach()
    endif()
endif()