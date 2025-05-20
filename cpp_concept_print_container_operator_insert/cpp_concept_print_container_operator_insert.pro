TEMPLATE = app
CONFIG += console
#TEMPLATE = lib
#CONFIG += staticlib

CONFIG -= app_bundle
CONFIG -= qt
#CONFIG += c++latest    # 2a
#CONFIG += c++23        # no effect?
#CONFIG += c++20        # 2a
CONFIG += warn_on
CONFIG += thread
#TARGET = eigen_playground
# ??CONFIG += QMAKE_CFLAGS_WARN_ON
# ??CONFIG += QMAKE_CXXFLAGS_WARN_ON
# // Compiler to use (comment out to use default one).
#DEFINES =
#QMAKE_LFLAGS += -static  # actually makes a static object.
#CONFIG += static wrong? why?
#QMAKE_CXX = clang++
QMAKE_CXXFLAGS += \
        #-std=c++23		    \
        -std=c++26		    \
        -O0 		            \
        -g3 		            \
        #-ggdb 		            \
        #-ggdb3 	            \
        -fconcepts              \
        #-fmodules-ts            \
        #-pedantic              \
        #-pedantic-errors       \
        -Wall   		        \  # https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html
        -Wextra   		        \
        -Wdeprecated-declarations\
        #-Weffc++               \
        #-Wno-comment 		    \
        #-Wno-uninitialized 	\
        #-Wno-reorder 		    \
        #-Wno-unused-parameter 	\
        #-Wno-unused-variable   \
        #-std=c++17             \
        #-std=c++1z             \
        #-std=gnu++17           \
        #-std=gnu++1z           \
        #-std=c++2a             \
        #-std=gnu++2a           \
        #-std=c++20             \
        #-std=gnu++20           \
        #-std=c++2b             \
        #-std=gnu++2b           \
        #-DBOOST_LOG_DYN_LINK   \  		# on compile line not link line.
        #-DBOOST_SYSTEM_NO_DEPRECATED \  	# not sure what it does.

LIBS += \
        -lpthread               \
        -lrt                    \
        #-lstdc++_libbacktrace   \
        #-lboost_system         \
        #-lboost_coroutine      \
        #-lssl                  \  #  missing this still: undefined reference to symbol 'd2i_PrivateKey_bio@@OPENSSL_1_1_0  related to: OPENSSL_API_COMPAT
        #-lcrypto \  #  seems to be needed with -lssl

#INCLUDEPATH += $$PWD/../lib_tty
#INCLUDEPATH += $$PWD/.
#DEPENDPATH  += $$PWD/../lib_tty
#SUBDIRS +=
#if (!include(../../config.txt)) {
#	error("Failed to open config.txt")
#}
#HEADERS += \
#       $$PWD/..h               \ # probably wrong

SOURCES += \
    class_with_elements.cpp \
    main.cpp \

HEADERS += \
    class_with_elements.hpp

