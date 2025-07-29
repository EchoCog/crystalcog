cmake_minimum_required(VERSION 2.8.12.2)

project(opencog)

# In monorepo structure, we need to build dependencies first
# find_package(CogUtil 2.0.1 CONFIG REQUIRED)
find_package(AtomSpace 5.0.3 CONFIG REQUIRED)
find_package(AttentionBank 1.0.0 CONFIG)
find_package(URE 1.0.0 CONFIG)
find_package(PLN CONFIG)
find_package(LGAtomese 1.0.0 CONFIG)
find_package(Boost 1.46 COMPONENTS system REQUIRED)
find_package(Cxxtest)
include(OpenCogFindGuile)
include(OpenCogFindPython)
find_package(VALGRIND)
find_package(Doxygen)
