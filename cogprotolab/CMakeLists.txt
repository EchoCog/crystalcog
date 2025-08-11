# CogProtoLab - experimental cognitive protocols
cmake_minimum_required(VERSION 3.10)

project(cogprotolab)

# Install all CogProtoLab files
file(GLOB_RECURSE PROTO_FILES "*")
if(PROTO_FILES)
    foreach(proto_file ${PROTO_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${proto_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${proto_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/cogprotolab/${rel_dir})
    endforeach()
endif()