include(OpenCogGccOptions)
#Bioscience
find_library(BIOSCIENCE_LIB
        NAME libbioscience-types.dylib libbioscience-types.so
        PATHS ${CMAKE_INSTALL_PREFIX}/lib/opencog
        NO_DEFAULT_PATH)

#Protobuff
set(protobuf_MODULE_COMPATIBLE ON CACHE BOOL "")
find_package(ProtoBuf)
if(ProtoBuf_FOUND)
    include_directories(${PROTOBUF_INCLUDE_DIR})
    message(STATUS "Using protobuf ${Protobuf_VERSION}")
else()
    message(STATUS "ProtoBuf not found - RPC functionality will be disabled")
endif()

# gRPC
find_package(gRPC CONFIG)
if(gRPC_FOUND)
    message(STATUS "Using gRPC ${gRPC_VERSION}")
else()
    message(STATUS "gRPC not found - RPC functionality will be disabled")
endif()

#gRPC C++ plugin
find_program(gRPC_CPP_PLUGIN_EXECUTABLE grpc_cpp_plugin)

#Proto file
get_filename_component(atom_proto "${PROJECT_SOURCE_DIR}/specs/atom_server.proto" ABSOLUTE)
get_filename_component(atom_proto_path "${atom_proto}" DIRECTORY)
#Generated sources
if(ProtoBuf_FOUND)
    PROTOBUF_GENERATE_CPP(atom_proto_srcs  atom_proto_hdrs "${PROJECT_SOURCE_DIR}/specs/atom_server.proto")
    #protobuf_generate_cpp(atom_proto_srcs atom_proto_hdrs "${atom_proto}")
    set(atom_grpc_srcs "${CMAKE_CURRENT_BINARY_DIR}/atom_server.grpc.pb.cc")
    set(atom_grpc_hdrs "${CMAKE_CURRENT_BINARY_DIR}/atom_server.grpc.pb.h")
else()
    set(atom_proto_srcs "")
    set(atom_proto_hdrs "")
    set(atom_grpc_srcs "")
    set(atom_grpc_hdrs "")
endif()

if(ProtoBuf_FOUND AND gRPC_FOUND)
    add_custom_command(
            OUTPUT "${atom_grpc_srcs}" "${atom_grpc_hdrs}"
            COMMAND protobuf::protoc
            ARGS --grpc_out "${CMAKE_CURRENT_BINARY_DIR}" -I "${atom_proto_path}"
            --cpp_out "${CMAKE_CURRENT_BINARY_DIR}"
            --plugin=protoc-gen-grpc="${gRPC_CPP_PLUGIN_EXECUTABLE}"
            "${atom_proto}"
            DEPENDS "${atom_proto}"
    )
endif()

#Generated include directory
include_directories("${CMAKE_CURRENT_BINARY_DIR}")

set(SOURCES manager/AtomSpaceManager.cpp ${atom_grpc_srcs} ${atom_proto_srcs})

add_executable(atom_server AtomspaceService.cpp)
target_sources(atom_server PRIVATE ${SOURCES})

add_library(atom_service_client SHARED
        AtomServiceClient.cpp
        manager/AtomSpaceManager.cpp ${atom_grpc_srcs} ${atom_proto_srcs})

# Set default include paths.
include_directories(${CMAKE_CURRENT_SOURCE_DIR}
        ${COGUTIL_INCLUDE_DIR}
        ${ATOMSPACE_INCLUDE_DIR}
        ${GUILE_INCLUDE_DIR})


target_link_libraries(atom_server
        ${COGUTIL_LIBRARIES}
        ${ATOMSPACE_LIBRARIES}
        ${Boost_LIBRARIES}
        protobuf::libprotobuf
        gRPC::grpc++_unsecure)


target_link_libraries(atom_service_client
        ${COGUTIL_LIBRARIES}
        ${ATOMSPACE_LIBRARIES}
        ${Boost_LIBRARIES}
        ${GUILE_LIBRARIES}
        protobuf::libprotobuf
        gRPC::grpc++_unsecure)

IF(BIOSCIENCE_LIB)
    message(STATUS "AGI-Bio found.")
    add_definitions(-DHAVE_BIOSCIENCE)
    set(HAVE_BIOSCIENCE 1)
    target_link_libraries(atom_server
            ${NO_AS_NEEDED}
            ${BIOSCIENCE_LIB})
    target_link_libraries(atom_service_client
            ${NO_AS_NEEDED}
            ${BIOSCIENCE_LIB})

ENDIF()

install(TARGETS atom_server DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)

install(TARGETS atom_service_client
        DESTINATION "lib${LIB_DIR_SUFFIX}/opencog")

add_subdirectory(guile)
