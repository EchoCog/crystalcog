# Unity3D OpenCog Game
cmake_minimum_required(VERSION 3.10)

project(unity3d-opencog-game)

# Install Unity3D game files
file(GLOB_RECURSE UNITY_FILES "*")
if(UNITY_FILES)
    foreach(unity_file ${UNITY_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${unity_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${unity_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/unity3d-opencog-game/${rel_dir})
    endforeach()
endif()