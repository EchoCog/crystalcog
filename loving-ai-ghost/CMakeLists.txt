# Loving AI GHOST
cmake_minimum_required(VERSION 3.10)

project(loving-ai-ghost)

# Install Loving AI GHOST files
file(GLOB_RECURSE GHOST_FILES "*")
if(GHOST_FILES)
    foreach(ghost_file ${GHOST_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${ghost_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${ghost_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/loving-ai-ghost/${rel_dir})
    endforeach()
endif()