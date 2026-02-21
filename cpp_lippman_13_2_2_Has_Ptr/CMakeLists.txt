cmake_minimum_required(VERSION 3.16)

project(cpp_lippman_13_2_2_Has_Ptr LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 26)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(cpp_lippman_13_2_2_Has_Ptr main.cpp)

include(GNUInstallDirs)
install(TARGETS cpp_lippman_13_2_2_Has_Ptr
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
