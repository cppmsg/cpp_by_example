# Copyright (c) Grant Rostig, grantrostig.com, Boost 1.0 license. Either these three or the next 2, for app or lib, respectively.
 TEMPLATE = app                 # comment out these 3,
 CONFIG +=  console

 CONFIG -= app_bundle
 CONFIG -= qt


QMAKE_CXXFLAGS += \                     #https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/
    -std=c++26		            \
    -g3 		                \
    -O0 		                \
    -fconcepts 	                \
    -Wall   		            \       # https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html \
    -Wextra   		            \

LIBS += \                               # https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/Link-Options.html
   #-lgrostig_tools             \
    -L$$PWD/.                   -lgrostig_tools \     # be sure we built it as a library, not as a test program with main.cpp
   #-L$$PWD/../grostig_tools/.  -llibgrostig_tools.a \     # be sure we built it as a library, not as a test program with main.cpp
    -lstdc++exp                 \

HEADERS +=                          \
    global_entities.hpp             \
    dual_bindage_component.hpp      \
    external_linkage_component.hpp  \
   #ut.hpp                          \
   #$$PWD/..h                       \   # probably wrong
    internal_linkage_component.hpp  \
    no_linkage_component.hpp        \

SOURCES +=                          \
    dual_bindage_component.cpp      \
    external_linkage_component.cpp  \
    internal_linkage_component.cpp  \
    no_linkage_component.cpp        \
    main.cpp                        \  # we add SOURCES others below.
