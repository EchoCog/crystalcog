# OpenCog website and documentation
cmake_minimum_required(VERSION 3.10)

project(opencog-org)

# Install website files
file(GLOB_RECURSE WEB_FILES "*.html" "*.css" "*.js" "*.png" "*.jpg" "*.gif" "*.svg")
if(WEB_FILES)
    foreach(web_file ${WEB_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${web_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${web_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/website/${rel_dir})
    endforeach()
endif()

# Install documentation files
file(GLOB DOC_FILES "*.md" "*.txt")
if(DOC_FILES)
    install(FILES ${DOC_FILES}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/website)
endif()