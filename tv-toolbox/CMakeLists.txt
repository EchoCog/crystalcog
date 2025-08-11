# TV-Toolbox - Truth Value tools
cmake_minimum_required(VERSION 3.10)

project(tv-toolbox)

# Install TV-Toolbox files
file(GLOB_RECURSE TV_FILES "*")
if(TV_FILES)
    foreach(tv_file ${TV_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${tv_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${tv_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/tv-toolbox/${rel_dir})
    endforeach()
endif()