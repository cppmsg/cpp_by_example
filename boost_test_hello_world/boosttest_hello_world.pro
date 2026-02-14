TEMPLATE = app
CONFIG += console c++latest warn_on
CONFIG -= app_bundle
CONFIG -= qt
# ??CONFIG += QMAKE_CFLAGS_WARN_ON
# ??CONFIG += QMAKE_CXXFLAGS_WARN_ON

QMAKE_CXXFLAGS += \
        #-std=gnu++2b		\
        -g3 		        \
        -O0 		        \
        -Wall   		\  # https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html
        -Wextra   		\
        #-Wno-comment 		\
        #-Wno-unused-parameter 	\
        #-Wno-uninitialized 	\
        #-Wno-reorder 		\

LIBS += \
        -lpthread \
        -lstdc++_libbacktrace \
        -lrt                    \
        -lboost_unit_test_framework

SOURCES += \
    #main_stable.cpp                 \
    main_ut.cpp                 \

HEADERS += \

# Default rules for deployment.
#unix {
    #target.path = $$[QT_INSTALL_PLUGINS]/generic
#}
#!isEmpty(target.path): INSTALLS += target

DISTFILES += \
    LICENSE.txt \
    README.txt
