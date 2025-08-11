# Docker directory - containerization files
cmake_minimum_required(VERSION 3.10)

# Install Docker files for reference
file(GLOB DOCKER_FILES "Dockerfile*" "docker-compose*.yml" "*.sh")
if(DOCKER_FILES)
    install(FILES ${DOCKER_FILES}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/docker)
endif()

# Install subdirectories if they exist
file(GLOB DOCKER_SUBDIRS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *)
foreach(subdir ${DOCKER_SUBDIRS})
    if(IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/${subdir}")
        install(DIRECTORY ${subdir}/
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/docker/${subdir})
    endif()
endforeach()