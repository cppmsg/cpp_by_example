# Copyright (c) Grant Rostig, grantrostig.com, Boost 1.0 license
 TEMPLATE = app                 # comment out these 3,
 SOURCES  = main.cpp           # we add SOURCES others below.
 CONFIG +=  console
 CONFIG -= app_bundle
 CONFIG -= qt

QMAKE_CXXFLAGS += \                     #https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/
    -std=c++23		            \
    -g3 		                \
    -O0 		                \
    -pedantic                   \
    -pedantic-errors            \
    -Wall   		            \       # https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html \
    -Wextra   		            \

LIBS += \                               # https://gcc.gnu.org/onlinedocs/gcc-13.2.0/gcc/Link-Options.html
   #-lpthread                   \
   #-lrt                        \
   #-lstdc++exp                 \-rw-r--r--. 1 root root   67041558 Feb  7  2024 -lwx_baseu    .a
   #-lwx_baseu_net              \
   #-lwx_baseu_xml              \
   #-lwxexpat                   \
   #-lwx_gtk3u_adv              \
   #-lwx_gtk3u_aui              \
   #-lwx_gtk3u_core             \
   #-lwx_gtk3u_gl               \
   #-lwx_gtk3u_html             \
   #-lwx_gtk3u_propgrid         \
   #-lwx_gtk3u_qa               \
   #-lwx_gtk3u_ribbon           \
   #-lwx_gtk3u_richtext         \
   #-lwx_gtk3u_stc              \
   #-lwx_gtk3u_xrc              \
   #-lwxscintilla               \
   #-lxtd                       \
   #-lxtd.core                  \
    -lxtd.cored                 \
   #-lxtd.core.native.linux     \
    -lxtd.core.native.linuxd    \
    -lxtdd                      \
    -lxtd.drawing               \
   #-lxtd.drawingd              \
    -lxtd.drawing.native.wxwidgets\
   #-lxtd.drawing.native.wxwidgetsd\
    -lxtd.forms                 \
   #-lxtd.formsd                \
    -lxtd.forms.native.wxwidgets\
   #-lxtd.forms.native.wxwidgetsd\
    -lxtd.tunit                 \
   #-lxtd.tunitd                \


HEADERS +=                      \
    controller.hpp              \
    model.hpp                   \
    view_main_form.hpp

SOURCES +=                      \
   #global_entities.cpp         \
