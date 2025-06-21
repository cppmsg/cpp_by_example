cmake_minimum_required(VERSION 3.20)

# Project
project(xtd_mvc)
find_package(xtd REQUIRED)
add_sources(
  src/model.hpp
  src/controller.hpp
  src/view_main_form.hpp
  src/main.cpp
)
target_type(GUI_APPLICATION)

