# Yacc Grammar Implementation
cmake_minimum_required(VERSION 3.10)

# Create the Yacc grammar library
add_library(yacc_grammar
    lexer.cpp
    parser.cpp
    ast.cpp
    code_generator.cpp
    template_system.cpp
    validator.cpp
    yacc_grammar_system.cpp
)

target_include_directories(yacc_grammar PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../../include>
    $<INSTALL_INTERFACE:include>
    ${Boost_INCLUDE_DIRS}
)

target_link_libraries(yacc_grammar
    ${Boost_LIBRARIES}
    Threads::Threads
)

# Set library properties
set_target_properties(yacc_grammar PROPERTIES
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED ON
)

# Install target
install(TARGETS yacc_grammar
    EXPORT DrawTermTargets
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib
)