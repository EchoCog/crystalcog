# REST API Documentation - web documentation
cmake_minimum_required(VERSION 3.10)

# Install documentation files
file(GLOB DOC_FILES "*.html" "*.js" "*.css" "*.json")
if(DOC_FILES)
    install(FILES ${DOC_FILES}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/rest-api)
endif()

# Install subdirectories
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/lib")
    install(DIRECTORY lib/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/rest-api/lib)
endif()

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/images")
    install(DIRECTORY images/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/rest-api/images)
endif()

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/css")
    install(DIRECTORY css/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/rest-api/css)
endif()