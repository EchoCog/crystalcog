# OpenCog Debian packaging
cmake_minimum_required(VERSION 3.10)

project(opencog-debian)

# Install debian packaging files
file(GLOB_RECURSE DEBIAN_FILES "*")
if(DEBIAN_FILES)
    foreach(debian_file ${DEBIAN_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${debian_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${debian_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/packaging/debian/${rel_dir})
    endforeach()
endif()