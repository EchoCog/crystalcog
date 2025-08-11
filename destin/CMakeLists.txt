# DeSTIN - Deep Spatial-Temporal Inference Network
cmake_minimum_required(VERSION 3.10)

project(destin)

# Check if this has existing CMakeLists.txt structure  
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt" AND NOT "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt" STREQUAL "${CMAKE_CURRENT_LIST_FILE}")
    # Include existing CMakeLists.txt if different from this one
    include("${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt")
else()
    # Install DeSTIN files
    file(GLOB_RECURSE DESTIN_FILES "*")
    if(DESTIN_FILES)
        foreach(destin_file ${DESTIN_FILES})
            file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${destin_file})
            get_filename_component(rel_dir ${rel_path} DIRECTORY)
            install(FILES ${destin_file}
                    DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/destin/${rel_dir})
        endforeach()
    endif()
endif()