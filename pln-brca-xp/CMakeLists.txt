# PLN BRCA Experiment
cmake_minimum_required(VERSION 3.10)

project(pln-brca-xp)

# Install PLN BRCA experiment files
file(GLOB_RECURSE PLN_FILES "*")
if(PLN_FILES)
    foreach(pln_file ${PLN_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${pln_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${pln_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/pln-brca-xp/${rel_dir})
    endforeach()
endif()