# OpenCog Raspberry Pi tools
cmake_minimum_required(VERSION 3.10)

project(opencog_rpi)

# Install toolchain and CMake files
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/arm_gnueabihf_toolchain.cmake")
    install(FILES arm_gnueabihf_toolchain.cmake
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/opencog_rpi)
endif()

if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/cmake")
    install(DIRECTORY cmake/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/opencog_rpi/cmake)
endif()

# Install dependency lists
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/dep_lists")
    install(DIRECTORY dep_lists/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/opencog_rpi/dep_lists)
endif()

# Install tools
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/tools-master")
    install(DIRECTORY tools-master/
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/opencog_rpi/tools-master)
endif()

# Install scripts with executable permissions
file(GLOB SCRIPT_FILES "*.sh")
if(SCRIPT_FILES)
    install(FILES ${SCRIPT_FILES}
            DESTINATION ${CMAKE_INSTALL_BINDIR}
            PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)
endif()

# Install documentation
if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
    install(FILES README.md
            DESTINATION ${CMAKE_INSTALL_DATADIR}/opencog/doc/opencog_rpi)
endif()