# Link Grammar Website
cmake_minimum_required(VERSION 3.10)

project(link-grammar-website)

# Install website files
file(GLOB_RECURSE WEB_FILES "*.html" "*.css" "*.js" "*.png" "*.jpg" "*.gif" "*.svg" "*.md")
if(WEB_FILES)
    foreach(web_file ${WEB_FILES})
        file(RELATIVE_PATH rel_path ${CMAKE_CURRENT_SOURCE_DIR} ${web_file})
        get_filename_component(rel_dir ${rel_path} DIRECTORY)
        install(FILES ${web_file}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/link-grammar-website/${rel_dir})
    endforeach()
endif()