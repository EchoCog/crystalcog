# Test datasets - data for testing and examples
cmake_minimum_required(VERSION 3.10)

project(test-datasets)

# Install all test datasets
file(GLOB DATASET_DIRS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} *)
foreach(dataset_dir ${DATASET_DIRS})
    if(IS_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/${dataset_dir}")
        install(DIRECTORY ${dataset_dir}/
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/test-datasets/${dataset_dir})
    endif()
endforeach()

# Install documentation
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
    install(FILES README.md
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/test-datasets)
endif()