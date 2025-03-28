TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG -= qt
#CONFIG += c++latest
#CONFIG += c++20
CONFIG += warn_on
# ??CONFIG += QMAKE_CFLAGS_WARN_ON
# ??CONFIG += QMAKE_CXXFLAGS_WARN_ON

QMAKE_CXXFLAGS += \
# 2b could be wrong:
       #-std=gnu++26 		\
        -std=c++26 		\
        -g3 		        \
        -O0 		        \
#        -Wno-comment 		\
#        -Wno-uninitialized 	\
#        -Wno-reorder 		\
#        -Wno-unused-parameter 	\

LIBS += \
        -lpthread 		\
        -lrt 			\
       #-lstdc++_libbacktrace 	\
        -L /home/grostig/libs/lib \

#INCLUDEPATH +=../../cereal/include/
#INCLUDEPATH +=/home/grostig/libs/include/
INCLUDEPATH += $$PWD/../lib_tty
#DEPENDPATH +=  $$PWD/../lib_tty

SOURCES += \
    ./src/main.cpp

HEADERS += \

