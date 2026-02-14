# Original project name and dir: qt_QtQuickApplication_template_cmake
cmake_minimum_required(VERSION 3.16)
project(qt_QtQuickApplication_template_cmake VERSION 0.1 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Qt6 REQUIRED COMPONENTS Quick)
find_package(Qt6 REQUIRED COMPONENTS Core Widgets Quick)

qt_standard_project_setup(REQUIRES 6.8)

qt_add_executable(appqt_QtQuickApplication_template_cmake
    main.cpp
)

qt_add_qml_module(appqt_QtQuickApplication_template_cmake
    URI qt_QtQuickApplication_template_cmake
    VERSION 1.0
    QML_FILES
        Main.qml
        SOURCES gr_test_class_win_include_all_stuff.hpp gr_test_class_win_include_all_stuff.cpp
        SOURCES gr_test_class_cpp.hpp gr_test_class_cpp.cpp
)

# Qt for iOS sets MACOSX_BUNDLE_GUI_IDENTIFIER automatically since Qt 6.1.
# If you are developing for iOS or macOS you should consider setting an
# explicit, fixed bundle identifier manually though.
set_target_properties(appqt_QtQuickApplication_template_cmake PROPERTIES
#    MACOSX_BUNDLE_GUI_IDENTIFIER com.example.appqt_QtQuickApplication_template_cmake
    MACOSX_BUNDLE_BUNDLE_VERSION ${PROJECT_VERSION}
    MACOSX_BUNDLE_SHORT_VERSION_STRING ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
    MACOSX_BUNDLE TRUE
    WIN32_EXECUTABLE TRUE
)

target_link_libraries(appqt_QtQuickApplication_template_cmake
    PRIVATE Qt6::Quick
)
target_link_libraries(appqt_QtQuickApplication_template_cmake PRIVATE Qt6::Core Qt6::Widgets Qt6::Quick)

include(GNUInstallDirs)
install(TARGETS appqt_QtQuickApplication_template_cmake
    BUNDLE DESTINATION .
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
