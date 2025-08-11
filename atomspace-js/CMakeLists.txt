# AtomSpace JavaScript bindings
cmake_minimum_required(VERSION 3.10)

project(atomspace-js)

# Install JavaScript/Node.js files
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
    install(FILES README.md
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/atomspace-js)
endif()

# Install all JavaScript content
file(GLOB_RECURSE JS_FILES "*.js" "*.json" "*.md")
if(JS_FILES)
    foreach(js_file ${JS_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${js_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${js_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/atomspace-js/${rel_dir})
    endforeach()
endif()