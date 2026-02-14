TEMPLATE = app
CONFIG += console c++20
#QMAKE_LFLAGS += -static  # actually makes a static object.
#CONFIG += static wrong? why?
CONFIG -= app_bundle
CONFIG -= qt
CONFIG -= debug

SOURCES += \
        main.cpp

LIBS += \
        -lboost_system \
        -pthread \
        -lssl \  #  missing this still: undefined reference to symbol 'd2i_PrivateKey_bio@@OPENSSL_1_1_0  related to: OPENSSL_API_COMPAT
        -lcrypto \  #  seems to be needed with -lssl
        -lboost_coroutine \

# maybe these are used if not using header-only library?
            #-lasio \
            #-lbeast
            #-llib-asio \
            #-llib-beast
