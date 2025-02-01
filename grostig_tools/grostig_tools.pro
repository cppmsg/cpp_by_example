# Copyright (c) Grant Rostig, grantrostig.com, Boost 1.0 license
# Snippet
#CONFIG += c++latest    # 2a
#CONFIG += c++23        # no effect?
#CONFIG += c++20        # 2a
#CONFIG += c++11

TEMPLATE = app
 CONFIG += cmdline
#CONFIG += console  # windowsonly
#TEMPLATE = lib
#CONFIG += staticlib
 CONFIG -= app_bundle
 CONFIG -= qt
 CONFIG += warn_on
#CONFIG += warn_off
 CONFIG += thread
 CONFIG += exceptions
#CONFIG += exceptions_off

#TARGET = eigen_playground
# ??CONFIG += QMAKE_CFLAGS_WARN_ON
# ??CONFIG += QMAKE_CXXFLAGS_WARN_ON
# // Compiler to use (comment out to use default one).
#DEFINES =
#QMAKE_LFLAGS += -static  # actually makes a static object.
#CONFIG += static wrong? why?
#QMAKE_CXX = clang++

QMAKE_CXXFLAGS += \                     #https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/
    #-fconcepts             \
    #-funsigned-char        \
                            \
    -std=c++26		        \
    #-std=c++23		        \
    #-std=gnu++23		    \
    -O0 		            \
    -g3 		            \
    #-ggdb 		            \
    #-ggdb3 	            \
    #-fmodules-ts           \
    -fconcepts              \
    #-pedantic              \
    #-pedantic-errors       \
    #-fsanitize=undefined   \       # 60K link errors possible \
    -Wall   		        \       # https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html \
    -Wextra   		        \
    -Weffc++                \
    -Wconversion            \
    -Wsign-conversion       \
    -Wdeprecated-declarations\
    #-Wno-comment 		    \
    #-Wno-uninitialized 	\
    #-Wno-reorder 		    \
    #-Wno-unused-parameter 	\
    #-Wno-unused-variable   \
    #-Wno-unused-const-variable \
    #-std=c++17             \       #https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/Link-Options.html
    #-std=gnu++17           \
    #-std=gnu++1z           \
    #-std=c++20             \
    #-std=gnu++20           \
    #-std=c++2a             \
    #-DBOOST_LOG_DYN_LINK   \  		# on compile line not link line.
    #-DBOOST_SYSTEM_NO_DEPRECATED \	# not sure what it does.

LIBS += \                               #https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/Link-Options.html
    -lstdc++exp                 \
   #-lssl                       \       # Requires:     -lcrypto
   #-lcrypto                    \       # Needed with:  -lssl
   #-lcryptopp                  \
   #-lpthread                   \
   #-lstdc++_libbacktrace       \       # allowed stack_trace to work in gcc 13, not 14, there needed -lstdc++exp
   #-lbson                      \
   #-lmongoc                    \
   #-lmysqlclient               \
                                \
   #-lboost_system              \
   #-lboost_coroutine           \
   #-lcurl                      \
   #-ldl                        \
   #-lrt                        \
   #-lz                         \
   #-llibz                      \
   #-lzlib                      \
   #-L/usr/lib64                \
   #--verbose                   \
                                \
#gcc libstdc++ Table 3.1. C++ Command Options
   #-std=c++98 or -std=c++03 	Use the 1998 ISO C++ standard plus amendments.
   #-std=gnu++98 or -std=gnu++03 	As directly above, with GNU extensions.
   #-std=c++11	Use the 2011 ISO C++ standard.
   #-std=gnu++11	As directly above, with GNU extensions.
   #-std=c++14	Use the 2014 ISO C++ standard.
   #-std=gnu++14	As directly above, with GNU extensions.
   #-fno-exceptions 	See exception-free dialect
   #-fno-rtti 	As above, but RTTI-free dialect.
   #-pthread	For ISO C++11 <thread>, <future>, <mutex>, or <condition_variable>.
   #-latomic	Linking to libatomic is required for some uses of ISO C++11 <atomic>.
   #-lstdc++exp	Linking to libstdc++exp.a is required for use of experimental C++ library features. This currently provides support for the C++23 types defined in the <stacktrace> header, the Filesystem library extensions defined in the <experimental/filesystem> header, and the Contracts extensions enabled by -fcontracts.
   #-lstdc++fs	Linking to libstdc++fs.a is another way to use the Filesystem library extensions defined in the <experimental/filesystem> header. The libstdc++exp.a library also provides all the symbols contained in this library.
   #-fopenmp	For parallel mode.
   #-ltbb	Linking to tbb (Thread Building Blocks) is required for use of the Parallel Standard Algorithms and execution policies in <execution>.
   #-ffreestanding	Limits the library to its freestanding subset. Headers that are not supported in freestanding will emit a "This header is not available in freestanding mode" error. Headers that are in the freestanding subset partially will not expose functionality that is not part of the freestanding subset.

#INCLUDEPATH += $$PWD/../lib_tty
#INCLUDEPATH += $$PWD/.
#DEPENDPATH  += $$PWD/../lib_tty
#SUBDIRS +=
#if (!include(../../config.txt)) {
#	error("Failed to open config.txt")
#}

HEADERS += \
    boost_headers.hpp \
    catch_amalgamated.hpp \
    cpp_headers.hpp \
    global_entities.hpp \
    gnuplot-iostream.hpp \
    math_grostig.hpp \
    ostream_joiner_gr.hpp \
    random_toolkit.hpp \
    ut.hpp                  \
    README.txt              \
#    KitchenSink.h           \
#    $$PWD/..h               \ # probably wrong

SOURCES += \
    main.cpp \
    global_entities.cpp \
    iostreams.cpp \
    math_grostig.cpp \
    ostream_joiner_gr.cpp \
    random_toolkit.cpp \

####################################################

# // Boost specific defines.
#DEFINES += BOOST_THREAD_VERSION=4
#DEFINES += BOOST_ASIO_DISABLE_STD_ATOMIC=1          # Workaround for GCC-4.6 issues with atomics
#DEFINES += BOOST_ASIO_DISABLE_HANDLER_TYPE_REQUIREMENTS=1 # Workaround for move-only functors support for ASIO
#isEmpty(BOOST_PATH) {
#    # Path to the Boost.
#    #BOOST_PATH = /data/boost_1_63_0/
#    BOOST_PATH = /data/boost/
#    #BOOST_PATH = /home/antoshkka/experiments/boost_1_63_0/
#}
# Paths to boost headers and libraries
# (if you are using Linux and installed Boost from repository you may comment out those lines).
#INCLUDEPATH += $$BOOST_PATH
#QMAKE_RPATHDIR += $$BOOST_PATH/stage/lib

# boost on fedora26, which is a parial list only derived from the Unbuntu list below.
#LIBS += \
#       -lboost_atomic \
#      -lboost_chrono \
#      -lboost_context \
#      -lboost_coroutine \
#      -lboost_date_time \
#      -lboost_fiber \
#      -lboost_filesystem \
#        -lboost_graph_parallel \
#      -lboost_graph \
#      -lboost_iostreams \
#      -lboost_locale \
#      -lboost_log_setup \
#      -lboost_log \
#      -lboost_math_c99f \
#      -lboost_math_c99l \
#      -lboost_math_c99 \
#      -lboost_math_tr1f \
#      -lboost_math_tr1l \
#      -lboost_math_tr1 \
#        -lboost_mpi_python-py27 \
#        -lboost_mpi_python-py35 \
#        -lboost_mpi_python \
#        -lboost_mpi \
#      -lboost_prg_exec_monitor \
#      -lboost_program_options \
#        -lboost_python-py27 \
#        -lboost_python-py35 \
#        -lboost_python \
#      -lboost_random \
#      -lboost_regex \
#      -lboost_serialization \
#        -lboost_signals \
#      -lboost_system \
#      -lboost_thread \
#      -lboost_timer \
#      -lboost_type_erasure \
#      -lboost_unit_test_framework \
#      -lboost_wave \
#      -lboost_wserialization

# boost on ubuntu 17.04
#LIBS += \
#        -lboost_atomic \
#        -lboost_chrono \
#        -lboost_context \
#        -lboost_coroutine \
#        -lboost_date_time \
#        -lboost_fiber \
#        -lboost_filesystem \
#        -lboost_graph_parallel \
#        -lboost_graph \
#        -lboost_iostreams \
#        -lboost_locale \
#        -lboost_log_setup \
#        -lboost_log \
#        -lboost_math_c99f \
#        -lboost_math_c99l \
#        -lboost_math_c99 \
#        -lboost_math_tr1f \
#        -lboost_math_tr1l \
#        -lboost_math_tr1 \
#        -lboost_mpi_python-py27 \
#        -lboost_mpi_python-py35 \
#        -lboost_mpi_python \
#        -lboost_mpi \
#        -lboost_prg_exec_monitor \
#        -lboost_program_options \
#        -lboost_python-py27 \
#        -lboost_python-py35 \
#        -lboost_python \
#        -lboost_random \
#        -lboost_regex \
#        -lboost_serialization \
#        -lboost_signals \
#        -lboost_system \
#        -lboost_thread \
#        -lboost_timer \
#        -lboost_type_erasure \
#        -lboost_unit_test_framework \
#        -lboost_wave \
#        -lboost_wserialization

# intersting switch
#!msvc:LIBS += -lboost_program_options

# Disabling some annoing warnings and adding paths to the Boost libraries.
#msvc {
#    QMAKE_LFLAGS += /LIBPATH:$$BOOST_PATH/stage/lib
#    DEFINES += _CRT_SECURE_NO_WARNINGS
#} else {
#    LIBS += -L$$BOOST_PATH/stage/lib
#}

DISTFILES += \
    catch_amalgamated.cpp.txt \
    CMakeLists.cmake \
    Makefile \
#    Qt/icons/gallery/index.theme \
#    Qt/icons/gallery/20x20/back.png \
#    Qt/icons/gallery/20x20/drawer.png \
#    Qt/icons/gallery/20x20/menu.png \
#    Qt/icons/gallery/20x20@2/back.png \
#    Qt/icons/gallery/20x20@2/drawer.png \
#    Qt/icons/gallery/20x20@2/menu.png \
#    Qt/icons/gallery/20x20@3/back.png \
#    Qt/icons/gallery/20x20@3/drawer.png \
#    Qt/icons/gallery/20x20@3/menu.png \
#    Qt/icons/gallery/20x20@4/back.png \
#    Qt/icons/gallery/20x20@4/drawer.png \
#    Qt/icons/gallery/20x20@4/menu.png \
#    Qt/icons/gallery/index.theme \
#    Qt/icons/gallery/20x20/back.png \
#    Qt/icons/gallery/20x20/drawer.png \
#    Qt/icons/gallery/20x20/menu.png \
#    Qt/icons/gallery/20x20@2/back.png \
#    Qt/icons/gallery/20x20@2/drawer.png \
#    Qt/icons/gallery/20x20@2/menu.png \
#    Qt/icons/gallery/20x20@3/back.png \
#    Qt/icons/gallery/20x20@3/drawer.png \
#    Qt/icons/gallery/20x20@3/menu.png \
#    Qt/icons/gallery/20x20@4/back.png \
#    Qt/icons/gallery/20x20@4/drawer.png \
#    Qt/icons/gallery/20x20@4/menu.png \
#    Qt/images/arrow.png \
#    Qt/images/arrow@2x.png \
#    Qt/images/arrow@3x.png \
#    Qt/images/arrow@4x.png \
#    Qt/images/arrows.png \
#    Qt/images/arrows@2x.png \
#    Qt/images/arrows@3x.png \
#    Qt/images/arrows@4x.png \
#    Qt/images/qt-logo.png \
#    Qt/images/qt-logo@2x.png \
#    Qt/images/qt-logo@3x.png \
#    Qt/images/qt-logo@4x.png \
#    README.txt
#    CMakeLists.cmake

# Default rules for deployment.
#unix {
    #target.path = $$[QT_INSTALL_PLUGINS]/generic
#}
#!isEmpty(target.path): INSTALLS += target

#unix: PRE_TARGETDEPS += $$PWD/../build-lib_tty-Desktop-Debug/liblib_tty.a

#win32:CONFIG(release, debug|release): LIBS += -L$$PWD/../build-lib_tty-Desktop_Qt_5_13_1_GCC_64bit-Debug/release/ -llib_tty
#else:win32:CONFIG(debug, debug|release): LIBS += -L$$PWD/../build-lib_tty-Desktop_Qt_5_13_1_GCC_64bit-Debug/debug/ -llib_tty
#else:unix: LIBS += -L$$PWD/../build-lib_tty-Desktop-Debug/ -llib_tty

# win32-g++:CONFIG(release, debug|release): PRE_TARGETDEPS += $$PWD/../build-lib_tty-Desktop_Qt_5_13_1_GCC_64bit-Debug/release/liblib_tty.a
# else:win32-g++:CONFIG(debug, debug|release): PRE_TARGETDEPS += $$PWD/../build-lib_tty-Desktop_Qt_5_13_1_GCC_64bit-Debug/debug/liblib_tty.a
# else:win32:!win32-g++:CONFIG(release, debug|release): PRE_TARGETDEPS += $$PWD/../build-lib_tty-Desktop_Qt_5_13_1_GCC_64bit-Debug/release/lib_tty.lib
# else:win32:!win32-g++:CONFIG(debug, debug|release): PRE_TARGETDEPS += $$PWD/../build-lib_tty-Desktop_Qt_5_13_1_GCC_64bit-Debug/debug/lib_tty.lib
# else:unix: PRE_TARGETDEPS += $$PWD/../build-lib_tty-Desktop_Qt_5_13_1_GCC_64bit-Debug/liblib_tty.a


#mac:{
#INCLUDEPATH += /usr/local/opt/boost/include \
               #/usr/local/opt/eigen/include
#LIBS -= -lboost_log
#LIBS += -L/usr/local/opt/boost/lib/ \
        #-lboost_log-mt
#}

# Flags to set your compiler into C++03/C++11/C++14 mode.
#msvc {
#    # MSVC compilers
#    CPP03FLAG =
#    CPP11FLAG =
#    CPP14FLAG =
#} else {
#    # all other compilers
#    CPP03FLAG = -std=c++03
#    CPP11FLAG = -std=c++0x
#    CPP14FLAG = -std=c++14
#}

DISTFILES += \
    CMakeLists.cmake \
    Makefile
