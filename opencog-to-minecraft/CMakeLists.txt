# OpenCog to Minecraft integration
cmake_minimum_required(VERSION 3.10)

project(opencog-to-minecraft)

# Install Minecraft integration files
file(GLOB_RECURSE MC_FILES "*")
if(MC_FILES)
    foreach(mc_file ${MC_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${mc_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${mc_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/opencog-to-minecraft/${rel_dir})
    endforeach()
endif()