# Python OpenCog Client
cmake_minimum_required(VERSION 3.10)

project(python-client)

# Check for Python
find_program(PYTHON_EXECUTABLE python3)

if(PYTHON_EXECUTABLE)
    # Install Python modules
    install(FILES __init__.py configuration.py opencog.py graphics.py
            DESTINATION ${CMAKE_INSTALL_LIBDIR}/python3/site-packages/opencog_client)
    
    # Install examples and documentation
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/examples")
        install(DIRECTORY examples/
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/examples/python-client)
    endif()
    
    file(GLOB JUPYTER_FILES "*.ipynb")
    if(JUPYTER_FILES)
        install(FILES ${JUPYTER_FILES}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/examples/python-client)
    endif()
    
    file(GLOB EXAMPLE_FILES "example.py")
    if(EXAMPLE_FILES)
        install(FILES ${EXAMPLE_FILES}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/examples/python-client)
    endif()
else()
    message(STATUS "Python3 not found - Python client will not be installed")
endif()

# Install documentation
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
    install(FILES README.md
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/python-client)
endif()