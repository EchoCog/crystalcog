# OpenCog Package Manager
cmake_minimum_required(VERSION 3.10)

project(ocpkg)

# Install ocpkg files
file(GLOB_RECURSE OCPKG_FILES "*")
if(OCPKG_FILES)
    foreach(ocpkg_file ${OCPKG_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${ocpkg_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${ocpkg_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/ocpkg/${rel_dir})
    endforeach()
endif()