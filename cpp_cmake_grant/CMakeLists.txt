# Grant Rostig copyright, License: Boost 1.0
# https://cmake.org/cmake/help/latest/command/add_link_options.html#command:add_link_options
# https://cmake.org/cmake/help/latest/manual/cmake-properties.7.html
# https://cmake.org/cmake/help/latest/manual/cmake-variables.7.html
# **** Commonly used method to build Cmake projects from the command line. ***
# cd project
# cmake -B build/
# cmake --build build --verbose
# cmake --build . --verbose
# make VERBOSE=1 # not sure what this is?
cmake_minimum_required(VERSION 3.20...3.27)
set(CMAKE_CXX_STANDARD 23)
# set_property(CMAKE_CXX_STANDARD 23)  #TODO??:
if(CMAKE_CXX_STANDARD LESS 17)
   set(CMAKE_CXX_STANDARD 17)
endif()
set(CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CXX_EXTENSIONS OFF)

# new in 3.11 https://cmake.org/cmake/help/latest/prop_sf/COMPILE_OPTIONS.html#prop_sf:COMPILE_OPTIONS
set_source_files_properties(foo.cpp PROPERTIES COMPILE_OPTIONS "-Wno-unused-parameter;-Wno-missing-field-initializer")

#https://cmake.org/cmake/help/latest/prop_sf/COMPILE_OPTIONS.html#prop_sf:COMPILE_OPTIONS
add_compile_options( -Wall -Wextra -pedantic -pedantic-errors -Wno-unused-variable ) # -fsanitize=undefined)

set(CMAKE_VERBOSE_MAKEFILE ON)  # Doesn't seem to do anything ??
#add_compile_definitions("NO_DEBUG")

find_package( Boost 1.81 ) #find_package(Boost 1.78 COMPONENTS program_options REQUIRED)
include_directories( src, lib )
project( cpp_cmake_grant LANGUAGES CXX ) #project( <EXECUTABLE_NAME> LANGUAGES C CXX )
add_executable( cpp_cmake_grant
#       src/TimerUser.h
#       lib/fteng/signals.hpp
        global_entities.hpp
        global_entities.cpp
        main.cpp
        )

#https://cmake.org/cmake/help/latest/command/add_link_options.html#command:add_link_options
#add_link_options(<option> ...)  Does NOT Work.
#target_link_libraries(  cpp_cmake_grant -lstdc++_libbacktrace )
target_link_libraries(  cpp_cmake_grant -lstdc++exp)
#???string(APPEND CMAKE_EXE_LINKER_FLAGS " -fexceptions")
#target_link_libraries(<EXECUTABLE_NAME> Boost::program_options)  # macOS

########################  other quality stuff ??
set (CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${CMAKE_CURRENT_SOURCE_DIR}/cmake")
#include(ElementsConfigCommon)
option(ELEMENTS_BUILD_EXAMPLES "build Elements library examples" ON)
option(ELEMENTS_ENABLE_LTO "enable link time optimization for Elements targets" OFF)
set(ELEMENTS_HOST_UI_LIBRARY "" CACHE STRING "gtk, cocoa or win32")
option(ELEMENTS_HOST_ONLY_WIN7 "If host UI library is win32, reduce elements features to support Windows 7" OFF)
option(ENABLE_GIT_SUBMODULE_CHECK "Check and clone submodules when not available." ON)
if (ENABLE_GIT_SUBMODULE_CHECK)
   if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/.git")
      include(GitSubmoduleCheck)
      git_submodule_check(lib/artist)
      git_submodule_check(lib/external/asio)
      git_submodule_check(lib/infra)
   endif()
endif()
#add_subdirectory(lib)
#if (ELEMENTS_BUILD_EXAMPLES)
   #set(ELEMENTS_ROOT ${PROJECT_SOURCE_DIR})
   #add_subdirectory(examples)
#endif()
