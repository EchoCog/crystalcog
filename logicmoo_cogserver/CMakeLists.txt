# LogicMOO CogServer
cmake_minimum_required(VERSION 3.10)

project(logicmoo_cogserver)

# Install LogicMOO CogServer files
file(GLOB_RECURSE LOGICMOO_FILES "*")
if(LOGICMOO_FILES)
    foreach(logicmoo_file ${LOGICMOO_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${logicmoo_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${logicmoo_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/logicmoo_cogserver/${rel_dir})
    endforeach()
endif()