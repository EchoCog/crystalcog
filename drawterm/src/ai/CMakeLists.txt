# AI Models Implementation
cmake_minimum_required(VERSION 3.10)

# Create the AI models library
add_library(ai_models
    ai_model.cpp
    ggml_model.cpp
    rwkv_model.cpp
    conversation_session.cpp
    drawkern_ai_manager.cpp
    integration.cpp
)

target_include_directories(ai_models PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../include>
    $<INSTALL_INTERFACE:include>
    ${Boost_INCLUDE_DIRS}
)

target_link_libraries(ai_models
    ${Boost_LIBRARIES}
    Threads::Threads
)

# Optional GGML support
if(ENABLE_GGML)
    find_library(GGML_LIBRARY ggml)
    if(GGML_LIBRARY)
        target_link_libraries(ai_models ${GGML_LIBRARY})
        target_compile_definitions(ai_models PRIVATE ENABLE_GGML)
    endif()
endif()

# Set library properties
set_target_properties(ai_models PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED ON
)

# Install target
install(TARGETS ai_models
    EXPORT DrawTermTargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)