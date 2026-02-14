cmake_minimum_required(VERSION 3.16)

project(std_smart_ptrs LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(std_smart_ptrs main.cpp)

include(GNUInstallDirs)
install(TARGETS std_smart_ptrs
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
