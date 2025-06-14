cmake_minimum_required(VERSION 3.20)

# Project
project(xtd_mvc_orig)
find_package(xtd REQUIRED)
add_sources(
  src/counter_model.hpp
  src/counter_controller.hpp
  src/main_form.hpp
  src/main.cpp
)
target_type(GUI_APPLICATION)

