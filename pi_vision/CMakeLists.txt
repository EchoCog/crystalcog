# Pi Vision - Raspberry Pi computer vision
cmake_minimum_required(VERSION 3.10)

project(pi_vision)

# Install Pi Vision files
file(GLOB_RECURSE PI_FILES "*")
if(PI_FILES)
    foreach(pi_file ${PI_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${pi_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${pi_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/pi_vision/${rel_dir})
    endforeach()
endif()