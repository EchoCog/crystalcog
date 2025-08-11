# Stochastic Language Generation
cmake_minimum_required(VERSION 3.10)

project(stochastic-language-generation)

# Install stochastic language generation files
file(GLOB_RECURSE SLG_FILES "*")
if(SLG_FILES)
    foreach(slg_file ${SLG_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${slg_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${slg_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/stochastic-language-generation/${rel_dir})
    endforeach()
endif()