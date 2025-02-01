# Copyright (c) Grant Rostig, grantrostig.com, Boost 1.0 license
# Snippet
#CONFIG += c++latest
#CONFIG += c++20
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
        #-funsigned-char         \
        \
        -std=c++23		    \
        #-std=gnu++23		    \
        -O0 		            \
        -g3 		            \
        #-ggdb 		            \
        #-ggdb3 	            \
        -fconcepts              \
        #-pedantic              \
        #-pedantic-errors       \
        #-fsanitize=undefined #60K link errors possible   \
        -Wall   		        \  # https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html
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
        #-Wno-unused-const-variable   \
        #-std=c++17             \       #https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/Link-Options.html
        #-std=gnu++17           \
        #-std=gnu++1z           \
        #-std=c++20             \
        #-std=gnu++20           \
        #-std=c++2a             \
        #-DBOOST_LOG_DYN_LINK   \  		# on compile line not link line.
        #-DBOOST_SYSTEM_NO_DEPRECATED \  	# not sure what it does.

LIBS += \                               #https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/Link-Options.html
    -lpthread               \
    -lstdc++exp             \
   #-lstdc++_libbacktrace   \       # allowed stack_trace to work in gcc 13, not 14, there needed -lstdc++exp
   #-lbson                  \
   #-lmongoc                \
   #-lmysqlclient           \
   \
   #-lboost_system         \
   #-lboost_coroutine      \
   #-lcurl                  \
   #-lcrypto \  #  seems to be needed with -lssl
   #-ldl                    \
   #-lrt                    \
   #-lssl                  \  #  missing this still: undefined reference to symbol 'd2i_PrivateKey_bio@@OPENSSL_1_1_0  related to: OPENSSL_API_COMPAT
   #-lz                     \
   #-llibz                 \
   #-lzlib                 \
   #-L/usr/lib64            \
   #--verbose               \
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
#    boost_headers.h         \
#    cpp_headers.h           \
#    global_entities.h       \
#    gnuplot-iostream.h      \
#    KitchenSink.h           \
#    math_grostig.h          \
#    random_toolkit.h        \
 \#    $$PWD/..h               \ # probably wrong
    biterator.hpp

SOURCES += \
    biterator.cpp \
#   main.cpp \
    main.t.cpp
#    iostreams.cpp \
#    global_entities.cpp \
#    random_toolkit.cpp \
 \####################################################
