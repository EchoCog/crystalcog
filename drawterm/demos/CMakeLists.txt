# DrawTerm Demos
cmake_minimum_required(VERSION 3.10)

# Create demo executables
add_executable(demo_drawkern_complete
    demo_drawkern_complete.cpp
)

add_executable(demo_styx_client_server
    demo_styx_client_server.cpp
)

add_executable(demo_disvm_execution
    demo_disvm_execution.cpp
)

add_executable(demo_yacc_parsing
    demo_yacc_parsing.cpp
)

add_executable(demo_ai_workbench
    demo_ai_workbench.cpp
)

# Link all demos with DrawTerm libraries
set(DEMO_TARGETS
    demo_drawkern_complete
    demo_styx_client_server
    demo_disvm_execution
    demo_yacc_parsing
    demo_ai_workbench
)

foreach(DEMO_TARGET ${DEMO_TARGETS})
    target_link_libraries(${DEMO_TARGET}
        drawterm_integration
        styx_protocol
        disvm_engine
        yacc_grammar
        ai_models
        ${Boost_LIBRARIES}
        Threads::Threads
    )
    
    target_include_directories(${DEMO_TARGET} PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}/../include
        ${Boost_INCLUDE_DIRS}
    )
    
    set_target_properties(${DEMO_TARGET} PROPERTIES
        CXX_STANDARD 17
        CXX_STANDARD_REQUIRED ON
    )
endforeach()

# Install demo executables
install(TARGETS ${DEMO_TARGETS}
    RUNTIME DESTINATION bin
)