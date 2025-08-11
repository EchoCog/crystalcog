# External tools directory
cmake_minimum_required(VERSION 3.10)

# Check if this has its own CMakeLists.txt structure
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt" AND NOT "${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt" STREQUAL "${CMAKE_CURRENT_LIST_FILE}")
    # Include the existing CMakeLists.txt if different from this one
    include("${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt")
else()
    # Install external tools
    file(GLOB TOOL_FILES "*.sh" "*.py" "*.pl")
    if(TOOL_FILES)
        install(FILES ${TOOL_FILES}
                DESTINATION ${CMAKE_INSTALL_BINDIR}
                PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
    endif()
    
    # Install subdirectories
    file(GLOB TOOL_SUBDIRS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *)
    foreach(subdir ${TOOL_SUBDIRS})
        if(IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/${subdir}")
            install(DIRECTORY ${subdir}/
                    DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/external-tools/${subdir})
        endif()
    endforeach()
endif()