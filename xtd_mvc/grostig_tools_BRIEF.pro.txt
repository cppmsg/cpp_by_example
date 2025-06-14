# Copyright (c) Grant Rostig, grantrostig.com, Boost 1.0 license
# BRIEF version versus LONG
# Either these three or the next 2, for app or lib, respectively.
 TEMPLATE = app                 # comment out these 3,
 SOURCES  = main.cpp            # we add other SOURCES below.
 CONFIG +=  console
#TEMPLATE = lib                 # OR comment out these 2.
#CONFIG +=  staticlib

 CONFIG -= app_bundle
 CONFIG -= qt

QMAKE_CXXFLAGS += \                     #https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/
    -fconcepts                  \
   #-fmodules-ts                \
   #-std=c++26		            \
    -std=c++23		            \
    -g3 		                \
    -O0 		                \
    -Wall   		            \       # https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html \
    -Wextra   		            \

LIBS += \                               # https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/Link-Options.html
    -lstdc++exp                 \

HEADERS +=                      \
    global_entities.hpp         \
    ut.hpp                      \
   #$$PWD/..h                   \   # probably wrong

SOURCES +=                      \
   # NOT HERE main.cpp          \   # declared above for app, and not declared above for lib.  Therefore do NOT declare it here in either case.
    global_entities.cpp         \

HEADERS +=                      \
   #boost_headers.hpp           \
   #catch_amalgamated.hpp       \
   #cpp_headers.hpp             \
   #gnuplot-iostream.hpp        \
   #iostreams.hpp               \
   #math_grostig.hpp            \
   #ostream_joiner_gr.hpp       \
   #random_toolkit.hpp          \
   #ut.cppm                     \

SOURCES += \
   #boost_headers.cpp           \
   #catch_amalgamated.cpp       \
   #cpp_headers.cpp             \
   #gnuplot-iostream.cpp        \
   #iostreams.cpp               \
   #math_grostig.cpp            \
   #ostream_joiner_gr.cpp       \
   #random_toolkit.cpp          \
   #ut.cppm                     \

DISTFILES += \
    CMakeLists.grostig.txt      \
    Makefile.txt                \
    README.txt                  \
    example.pro.txt             \
    clang-format_grostig_qt.xml \
    grostig_tools_LONG.pro.txt  \
    grostig_tools_BRIEF.pro.txt \

DISTFILES += \
    boost_headers.hpp.txt       \
    boost_headers.cpp.txt       \
    catch_amalgamated.hpp.txt   \
    catch_amalgamated.cpp.txt   \
    cpp_headers.hpp.txt         \
    cpp_headers.cpp.txt         \
    gnuplot-iostream.hpp.txt    \
    gnuplot-iostream.cpp.txt    \
    iostreams.hpp.txt           \
    iostreams.cpp.txt           \
    math_grostig.hpp.txt        \
    math_grostig.cpp.txt        \
    ostream_joiner_gr.hpp.txt   \
    ostream_joiner_gr.cpp.txt   \
    random_toolkit.hpp.txt      \
    random_toolkit.cpp.txt      \
    ut.hpp.txt                  \
    ut.cppm.txt                  \


####################################################
