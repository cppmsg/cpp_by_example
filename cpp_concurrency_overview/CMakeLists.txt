cmake_minimum_required(VERSION 3.16)

project(cpp_concurrency_overview LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(cpp_concurrency_overview main.cpp)

include(GNUInstallDirs)
install(TARGETS cpp_concurrency_overview
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
