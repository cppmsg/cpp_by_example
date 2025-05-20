# Copyright (c) Grant Rostig, grantrostig.com, Boost 1.0 license
# Either these three or the next 2, for app or lib, respectively.
 SOURCES  = main.cpp           # we add SOURCES others below.
 TEMPLATE = app                 # comment out these 3,
 CONFIG +=  console
#TEMPLATE = lib                 # OR comment out these 2.
#CONFIG +=  staticlib

 CONFIG -= app_bundle
 CONFIG -= qt
 #CONFIG += c++23

QMAKE_CXXFLAGS += \                     #https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/
   #-std=c++26		            \
    -std=gnu++26		            \
    -g3 		                \
    -O0 		                \
    -Wall   		            \       # https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html \
    -Wextra   		            \

LIBS += \                               # https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/Link-Options.html
    -lstdc++exp                 \

HEADERS +=                      \
   #boost_headers.hpp           \
   #catch_amalgamated.hpp       \
   #cpp_headers.hpp             \
   #global_entities.hpp         \
   #gnuplot-iostream.hpp        \
   #math_grostig.hpp            \
   #ostream_joiner_gr.hpp       \
   #random_toolkit.hpp          \
   #ut.hpp                      \

SOURCES +=                      \
    #NOT HERE main.cpp          \   # Declared above for app, and not declared above for lib.  Therefore do NOT declare it here in either case.
   #global_entities.cpp         \
   #iostreams.cpp               \
   #math_grostig.cpp            \
   #ostream_joiner_gr.cpp       \
   #random_toolkit.cpp          \

DISTFILES += \
    README.txt \
    example.pro.txt
