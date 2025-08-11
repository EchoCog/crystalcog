# LinkGrammar-RelEx Web interface
cmake_minimum_required(VERSION 3.10)

project(linkgrammar-relex-web)

# Install web interface files
file(GLOB_RECURSE WEB_FILES "*")
if(WEB_FILES)
    foreach(web_file ${WEB_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${web_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${web_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/linkgrammar-relex-web/${rel_dir})
    endforeach()
endif()