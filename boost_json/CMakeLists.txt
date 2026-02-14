#[=====[
#]=====]
#[=====[ *** Comment BEGIN ***
# Non Qt, for C++ on Fedora and others?
# Grant Rostig copyright (c) 2025, License: Boost 1.0
# Replace variable <ALL_CAPS> with all caps name, removing angle brakets.
# Replace variable <Initial_caps> with all lowecase name link UNIX directory name  // we put a Caps letter just to show it is a variable to be edited by programmer.
# https://cmake.org/cmake/help/latest/command/add_link_options.html#command:add_link_options
# https://cmake.org/cmake/help/latest/manual/cmake-properties.7.html
# https://cmake.org/cmake/help/latest/manual/cmake-variables.7.html
# **** Commonly used method to build Cmake projects from the command line. ***
# $ cd <Project_root_dir>
# $ cmake -B build/
# OR
# $ cmake --build build --verbose
# OR
# $ cmake --build . --verbose
# ? $ make VERBOSE=1 # not sure what this is?
*** Comment END *** #]=====]

cmake_minimum_required(VERSION 3.30)  # cmake_minimum_required(VERSION 3.9.6...3.15.0)
set(CMAKE_CXX_STANDARD 23)                                          # set_property(CMAKE_CXX_STANDARD 23)  #TODO??: old?
 if(CMAKE_CXX_STANDARD LESS 17)
   set(CMAKE_CXX_STANDARD 17)
 endif()
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_VERBOSE_MAKEFILE ON)  # Doesn't seem to do anything ??
set(CXX_STANDARD 23)
set(CXX_STANDARD_REQUIRED ON)
set(CXX_EXTENSIONS OFF)

# Compile otions: https://cmake.org/cmake/help/latest/prop_sf/COMPILE_OPTIONS.html#prop_sf:COMPILE_OPTIONS
# TODO??: add_compile_definitions("NO_DEBUG")
add_compile_options( -Wall -Wextra -pedantic -pedantic-errors -Wno-unused-variable ) # -fsanitize=undefined)
# Compile New in 3.11 https://cmake.org/cmake/help/latest/prop_sf/COMPILE_OPTIONS.html#prop_sf:COMPILE_OPTIONS
set_source_files_properties(<NAME>.cpp PROPERTIES COMPILE_OPTIONS "-Wno-unused-parameter;-Wno-missing-field-initializer")
find_package( Boost 1.81 )                                  # find_package(Boost 1.81 COMPONENTS program_options REQUIRED)
include_directories( src, include )  # TODO??: does this include the subdirectories too?

project( boost_json VERSION 0.1 LANGUAGES CXX )
add_executable( boost_json
       main.cpp
       boost_headers.cpp boost_headers.hpp global_entities.cpp global_entities.hpp README.txt
       cpp_llm_prompt_chatgpt.json cpp_llm_prompt_claude.json cpp_llm_prompt_gemini.json
       #src/main.cpp
       #src/<>.h
       #include/<>.h
       #src/<>.cpp
)
# *** LINKING: https://cmake.org/cmake/help/latest/command/add_link_options.html#command:add_link_options
# TODO??: add_link_options(<option> ...)  Does NOT Work.
# TODO??: target_link_libraries(boost_json Boost::program_options)  # macOS
# TODO??: string(APPEND CMAKE_EXE_LINKER_FLAGS " -fexceptions")
target_link_libraries(  boost_json -lstdc++_exp) # Others for Fedora: -lstdc++_libbacktrace

#[=====[ *** Comment BEGIN *** *** Qt STUFF *** Original project name and dir: qt_QtQuickApplication_template_cmake
set(CMAKE_AUTOUIC ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

find_package(QT NAMES Qt6 REQUIRED COMPONENTS                    Core Widgets Quick LinguistTools)
find_package(QT${QT_VERSION_MAJOR} NAMES Qt6 REQUIRED COMPONENTS Core Widgets Quick LinguistTools)
set(TS_FILES designp_active_object_concurrency_console_en_US.ts)
qt_create_translation(QM_FILES ${CMAKE_SOURCE_DIR} ${TS_FILES})

qt_standard_project_setup(REQUIRES 6.9)

set(PROJECT_SOURCES
        main.cpp
      # <>.cpp
      # <>.hpp
      # <>.ui
      # ${TS_FILES}
)
qt_add_executable( boost_json
    MANUAL_FINALIZATION
    ${PROJECT_SOURCES}
)

# TODO??: what!? - Define target properties for Android with Qt 6 as: set_property(TARGET designp_active_object_concurrency_console APPEND PROPERTY QT_ANDROID_PACKAGE_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/android ) For more information, see https://doc.qt.io/qt-6/qt-add-executable.html#target-creation

qt_add_qml_module( appboost_json                 # TODO??: or maybe not app< prefix
    URI boost_json
    VERSION 1.0
    QML_FILES
        Main.qml
        SOURCES <1>.hpp <1>.cpp
        SOURCES <2>.hpp <2>.cpp
)

set_target_properties(appqt_QtQuickApplication_template_cmake PROPERTIES
    ${BUNDLE_ID_OPTION}
    MACOSX_BUNDLE_BUNDLE_VERSION ${PROJECT_VERSION}
    MACOSX_BUNDLE_SHORT_VERSION_STRING ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
    MACOSX_BUNDLE TRUE
    WIN32_EXECUTABLE TRUE
)

target_link_libraries( appboost_json PRIVATE
    Qt${QT_VERSION_MAJOR}::Core
    Qt${QT_VERSION_MAJOR}::Widgets
    Qt${QT_VERSION_MAJOR}::Quick
)
include(GNUInstallDirs)
install(TARGETS boost_json            # for sometimes appboost_json
    BUNDLE DESTINATION .
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
if(QT_VERSION_MAJOR EQUAL 6)
    qt_finalize_executable(designp_active_object_concurrency_console)
endif()
#]=====]

#[=====[ *** Comment BEGIN *** ***  OTHER quality stuff? TODO??:
set (CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH};${CMAKE_CURRENT_SOURCE_DIR}/cmake")
include(<Executable_name>ConfigCommon)
option(boost_json_BUILD_EXAMPLES "build <Executable_name> library examples" ON)
option(boost_json_ENABLE_LTO "enable link time optimization for <Executable_name> targets" OFF)
set(boost_json_HOST_UI_LIBRARY "" CACHE STRING "gtk, cocoa or win32")
option(boost_json_HOST_ONLY_WIN7 "If host UI library is win32, reduce <Executable_name> features to support Windows 7" OFF)
option(ENABLE_GIT_SUBMODULE_CHECK "Check and clone submodules when not available." ON)
if (ENABLE_GIT_SUBMODULE_CHECK)
   if (EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/.git")
      include(GitSubmoduleCheck)
      git_submodule_check(lib/artist)
      git_submodule_check(lib/external/asio)
      git_submodule_check(lib/infra)
   endif()
endif()
add_subdirectory(lib)



# if (EXECUTABLE_NAME>_BUILD_EXAMPLES)
   # set(EXECUTABLE_NAME>_ROOT ${PROJECT_SOURCE_DIR})
   # add_subdirectory(examples)
# endif()

#]=====]
