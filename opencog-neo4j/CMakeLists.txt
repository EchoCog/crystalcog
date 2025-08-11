# OpenCog Neo4j integration
cmake_minimum_required(VERSION 3.10)

project(opencog-neo4j)

# Check for Java and Maven
find_program(JAVA_EXECUTABLE java)
find_program(MVN_EXECUTABLE mvn)

if(JAVA_EXECUTABLE AND MVN_EXECUTABLE AND EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/pom.xml")
    # Build Java components with Maven
    add_custom_target(opencog-neo4j-java
        COMMAND ${MVN_EXECUTABLE} compile
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Building OpenCog Neo4j Java components"
    )
    
    # Install JAR if built
    install(CODE "
        file(GLOB JAR_FILES \"${CMAKE_CURRENT_SOURCE_DIR}/target/*.jar\")
        if(JAR_FILES)
            file(INSTALL \${JAR_FILES}
                 DESTINATION \"${CMAKE_INSTALL_LIBDIR}/opencog\"
                 USE_SOURCE_PERMISSIONS)
        endif()
    ")
else()
    message(STATUS "Java/Maven not found or no pom.xml - Java components will not be built")
endif()

# Check for Python
find_program(PYTHON_EXECUTABLE python3)
if(PYTHON_EXECUTABLE)
    file(GLOB PYTHON_FILES "*.py")
    if(PYTHON_FILES)
        install(FILES ${PYTHON_FILES}
                DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/opencog-neo4j)
    endif()
endif()

# Install source code
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/src")
    install(DIRECTORY src/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/opencog-neo4j/src)
endif()

# Install protobuf definitions
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/protobuf")
    install(DIRECTORY protobuf/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/opencog-neo4j/protobuf)
endif()

# Install documentation
file(GLOB DOC_FILES "*.md")
if(DOC_FILES)
    install(FILES ${DOC_FILES}
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/opencog-neo4j)
endif()