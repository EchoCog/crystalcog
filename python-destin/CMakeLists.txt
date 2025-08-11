# Python DeSTIN bindings
cmake_minimum_required(VERSION 3.10)

project(python-destin)

# Check for Python
find_program(PYTHON_EXECUTABLE python3)

if(PYTHON_EXECUTABLE)
    # Install Python modules and data
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/destin")
        install(DIRECTORY destin/
                DESTINATION ${CMAKE_INSTALL_LIBDIR}/python3/site-packages/destin)
    endif()
    
    # Install test data
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/cifar-test")
        install(DIRECTORY cifar-test/
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/test-data/cifar-test)
    endif()
    
    # Install Jupyter notebooks
    file(GLOB JUPYTER_FILES "*.ipynb")
    if(JUPYTER_FILES)
        install(FILES ${JUPYTER_FILES}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/examples/python-destin)
    endif()
    
    # Install other files
    file(GLOB TEXT_FILES "*.txt")
    if(TEXT_FILES)
        install(FILES ${TEXT_FILES}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/python-destin)
    endif()
else()
    message(STATUS "Python3 not found - Python DeSTIN will not be installed")
endif()

# Install documentation
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
    install(FILES README.md
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/python-destin)
endif()