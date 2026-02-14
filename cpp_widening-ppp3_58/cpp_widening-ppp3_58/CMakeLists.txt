# CMakeList.txt : CMake project for cpp_widening-ppp3_58, include source and define
# project specific logic here.
#

# Add source to this project's executable.
add_executable (cpp_widening-ppp3_58 "cpp_widening-ppp3_58.cpp" "cpp_widening-ppp3_58.h")

if (CMAKE_VERSION VERSION_GREATER 3.12)
  set_property(TARGET cpp_widening-ppp3_58 PROPERTY CXX_STANDARD 26)
endif()

# TODO: Add tests and install targets if needed.
