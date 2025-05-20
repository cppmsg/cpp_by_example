/** NOT PRODUCTION QUALITY CODE, just shows learning/teaching example, not real programming, don't copy this style, just playing around
   Copyright (c) 2021 Grant Rostig. All rights reserved.
   License: Boost v1.0
   >> Permission is granted to use these files during and in support of CppMSG meetup group events and projects. This is done in the name of: Eckankar, The Path of Spritual Freedom.

// Copyright Grant Rostig 2015 - 2021. Distributed under the Boost Software License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)

   Author: 		Grant Rostig
   Inspired by: cppreference.com
   Shows how to:
   - Just include everything until modules is working properly and easily. :)
   - includes a few simple debugging functions
   - includes a few high precision constants
 */

#pragma once
#include <bits/stdc++.h>
using std::cin; using std::cerr; using std::cout; using std::endl; using std::clog; // using namespace std;


#include <ratio>
template<typename Ratio>
constexpr double math_constant_pi() // pi pie archimedes ' constant.  https://stackoverflow.com/a/51609207/4484362
{
    long double const pi = 3.14159265358979323846264338327950288419716939937510582097494459230781640628620899862803482534211706798214L; // http://oeis.org/A000796/constant
    return static_cast<double>(pi * Ratio::num / Ratio::den);
}
template<typename Ratio>
constexpr double math_constant_e()  // ln() log() natural logarithm e constant
{
    long double const e = 2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746L;  // https://oeis.org/A001113
    return static_cast<double>(e * Ratio::num / Ratio::den);
}
template<typename Ratio>
constexpr double feet_per_meter()  // ln() log() natural logarithm e constant
{
    long double const e = 3.2808398950131233595800524934383202099737532808398950131233595800524934383202099L;  // http://oeis.org/A224233/constant
    // inches per meter  39.370078740157480314960629921259842519685039370078740157480314960629921259842519
    return static_cast<double>(e * Ratio::num / Ratio::den);
}
template<typename Ratio>
constexpr double speed_of_light_c()  // speed of light c in meters per second  m/sec
{
    long double const e = 299792458L;  // http://oeis.org/A224233/constant  NOTE: this is actually NOT a CONSTANT, but we say it is!!
    return static_cast<double>(e * Ratio::num / Ratio::den);
}
/* ******  C++ Standard Library header files
The interface of C++ standard library is defined by the following collection of header files

// *** Utilities library
#include <cstdlib>		// 	General purpose utilities: program control, dynamic memory allocation, random numbers, sort and search
#include <csignal>		// 	Functions and macro constants for signal management
#include <csetjmp>		// 	Macro (and function) that saves (and jumps) to an execution context
#include <cstdarg>		// 	Handling of variable length argument lists
#include <typeinfo>		// 	Runtime type information utilities
#include <typeindex>		// (since C++11) 	std::type_index
#include <type_traits>		// (since C++11) 	Compile-time type information
#include <bitset>		// 	std::bitset class template
#include <functional>		// 	Function objects, designed for use with the standard algorithms
#include <utility>		// 	Various utility components
#include <ctime>		// 	C-style time/date utilites
#include <chrono>		// (since C++11) 	C++ time utilites
#include <cstddef>		// 	typedefs for types such as size_t, NULL and others
#include <initializer_list>		// (since C++11) 	std::initializer_list class template
#include <tuple>		// (since C++11) 	std::tuple class template
// no gcc 6.3.1 support #include <any>			// (since C++17) 	std::any class template
// #include <any>			// (since C++17) 	std::any class template
#include <experimental/any>			// (library fundamentals TS) 	Standard libraries extensions
// #include <optional>		// (since C++17) 	std::optional class template
#include <experimental/optional>		// (library fundamentals TS) 	Standard libraries extensions
// no gcc 6.3.1 support #include <variant>		// (since C++17) 	std::variant class template
// *** Dynamic memory management
#include <new>			// 	Low-level memory management utilities
#include <memory>		// 	Higher level memory management utilities
#include <scoped_allocator>		// (since C++11) 	Nested allocator class
// no gcc 6.3.1 support #include <memory_resource>		// (since C++17) 	Polymorphic allocators and memory resources
// *** Numeric limits
#include <climits>		// 	limits of integral types
#include <cfloat>		// 	limits of float types
#include <cstdint>		// (since C++11) 	fixed-size types and limits of other types
#include <cinttypes>		// (since C++11) 	formatting macros , intmax_t and uintmax_t math and conversions
#include <limits>		// 	standardized way to query properties of arithmetic types
// *** Error handling
#include <exception>		// 	Exception handling utilities
#include <stdexcept>		// 	Standard exception objects
#include <cassert>		// 	Conditionally compiled macro that compares its argument to zero
#include <system_error>		// (since C++11) 	defines std::error_code, a platform-dependent error code
#include <cerrno>		// 	Macro containing the last error number
// *** Strings library
#include <cctype>		// 	functions to determine the type contained in character data
#include <cwctype>		// 	functions for determining the type of wide character data
#include <cstring>		// 	various narrow character string handling functions
#include <cwchar>		// 	various wide and multibyte string handling functions
#include <cuchar>		// (since C++11) 	C-style Unicode character conversion functions
#include <string>		// 	std::basic_string class template
// no gcc 6.3.1 support #include <string_view>		// (since C++17) 	std::basic_string_view class template
// *** Containers library
#include <array>		// (since C++11) 	std::array container
#include <vector>		// 	std::vector container
#include <deque>		// 	std::deque container
#include <list>			// 	std::list container
#include <forward_list>		// (since C++11) 	std::forward_list container
#include <set>			// 	std::set and std::multiset associative containers
#include <map>			// 	std::map and std::multimap associative containers
#include <unordered_set>		// (since C++11) 	std::unordered_set and std::unordered_multiset unordered associative containers
#include <unordered_map>		// (since C++11) 	std::unordered_map and std::unordered_multimap unordered associative containers
#include <stack>		// 	std::stack container adaptor
#include <queue>		// 	std::queue and std::priority_queue container adaptors
// *** Algorithms library
#include <algorithm>		// 	Algorithms that operate on containers
// no gcc 6.3.1 support #include <execution>		// (C++17) 	Predefined execution policies for parallel versions of the algorithms
// *** Iterators library
#include <iterator>		// 	Container iterators
// *** Numerics library
#include <cmath>		// 	Common mathematics functions
#include <complex>		// 	Complex number type
#include <valarray>		// 	Class for representing and manipulating arrays of values
#include <random>		// (since C++11) 	Random number generators and distributions
#include <numeric>		// 	Numeric operations on values in containers
#include <ratio>		// (since C++11) 	Compile-time rational arithmetic
#include <cfenv>		// (since C++11) 	Floating-point environment access functions
// *** Input/output library
#include <iosfwd>		// 	forward declarations of all classes in the input/output library
#include <ios>			// 	std::ios_base class, std::basic_ios class template and several typedefs
#include <istream>		// 	std::basic_istream class template and several typedefs
#include <ostream>		// 	std::basic_ostream, std::basic_iostream class templates and several typedefs
#include <iostream>		// 	several standard stream objects
#include <fstream>		// 	std::basic_fstream, std::basic_ifstream, std::basic_ofstream class templates and several typedefs
#include <sstream>		// 	std::basic_stringstream, std::basic_istringstream, std::basic_ostringstream class templates and several typedefs
//#include <strstream>		// 	std::strstream, std::istrstream, std::ostrstream(deprecated)
#include <iomanip>		// 	Helper functions to control the format or input and output
#include <streambuf>		// 	std::basic_streambuf class template
#include <cstdio>		// 	C-style input-output functions
// *** Localization library
#include <locale>		// 	Localization utilities
#include <clocale>		// 	C localization utilities
#include <codecvt>		// (since C++11) 	Unicode conversion facilities
// *** Regular Expressions library
#include <regex>		// (since C++11) 	Classes, algorithms and iterators to support regular expression processing
// *** Atomic Operations library
#include <atomic>		// (since C++11) 	Atomic operations library
// *** Thread support library
#include <thread>		// (since C++11) 	std::thread class and supporting functions
#include <mutex>		// (since C++11) 	mutual exclusion primitives
#include <shared_mutex>		// (since C++14) 	shared mutual exclusion primitives
#include <future>		// (since C++11) 	primitives for asynchronous computations
#include <condition_variable>	// (since C++11) 	thread waiting conditions
// *** Filesystem library
// no gcc 6.3.1 support #include <filesystem>		// (since C++17) 	std::path class and supporting functions
#include <experimental/filesystem>		// (filesystem TS) 	Filesystem library
*/
/* * ******** Experimental libraries
#include <experimental/algorithm>		// (library fundamentals TS) 	Standard libraries extensions and Extensions for Parallelism
#include <experimental/any>			// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/chrono>			// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/deque>			// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/execution_policy>	// (parallelism TS) 	Extensions for Parallelism
#include <experimental/exception_list>		// (parallelism TS) 	Extensions for Parallelism
#include <experimental/filesystem>		// (filesystem TS) 	Filesystem library
#include <experimental/forward_list>		// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/future>			// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/list>			// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/functional>		// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/map>			// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/memory>			// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/memory_resource>		// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/numeric>			// (parallelism TS) 	Extensions for Parallelism
#include <experimental/optional>		// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/ratio>			// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/regex>			// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/set>			// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/string>			// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/string_view>		// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/system_error>		// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/tuple>			// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/type_traits>		// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/unordered_map>		// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/unordered_set>		// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/utility>			// (library fundamentals TS) 	Standard libraries extensions
#include <experimental/vector>			// (library fundamentals TS) 	Standard libraries extensions
****** */
/* ****** C compatibility headers

For some of the C standard library headers of the form xxx.h, the C++ standard library both includes an identically-named header and another header of the form cxxx (all meaningful cxxx headers are listed above).

With the exception of complex.h, each xxx.h header included in the C++ standard library places in the global namespace each name that the corresponding cxxx header would have placed in the std namespace.

These headers are allowed to also declare the same names in the std namespace, and the corresponding cxxx headers are allowed to also declare the same names in the global namespace: including #include <cstdlib> definitely provides std::malloc and may also provide ::malloc. Including <stdlib.h> definitely provides ::malloc and may also provide std::malloc. This applies even to functions and function overloads that are not part of C standard library.
#include <assert.h>		// (deprecated) 	behaves as if each name from <cassert> is placed in global namespace
#include <ctype.h>		// (deprecated) 	behaves as if each name from <cctype> is placed in global namespace
#include <errno.h>		// (deprecated) 	behaves as if each name from <cerrno> is placed in global namespace
#include <fenv.h>		// (deprecated) 	behaves as if each name from <cfenv> is placed in global namespace
#include <float.h>		// (deprecated) 	behaves as if each name from <cfloat> is placed in global namespace
#include <inttypes.h>		// (deprecated) 	behaves as if each name from <cinttypes> is placed in global namespace
#include <limits.h>		// (deprecated) 	behaves as if each name from <climits> is placed in global namespace
#include <locale.h>		// (deprecated) 	behaves as if each name from <clocale> is placed in global namespace
#include <math.h>		// (deprecated) 	behaves as if each name from <cmath> is placed in global namespace
#include <setjmp.h>		// (deprecated) 	behaves as if each name from <csetjmp> is placed in global namespace
#include <signal.h>		// (deprecated) 	behaves as if each name from <csignal> is placed in global namespace
#include <stdarg.h>		// (deprecated) 	behaves as if each name from <cstdarg> is placed in global namespace
#include <stddef.h>		// (deprecated) 	behaves as if each name from <cstddef> is placed in global namespace
#include <stdint.h>		// (deprecated) 	behaves as if each name from <cstdint> is placed in global namespace
#include <stdio.h>		// (deprecated) 	behaves as if each name from <cstdio> is placed in global namespace
#include <stdlib.h>		// (deprecated) 	behaves as if each name from <cstdlib> is placed in global namespace
#include <string.h>		// (deprecated) 	behaves as if each name from <cstring> is placed in global namespace
#include <time.h>		// (deprecated) 	behaves as if each name from <ctime> is placed in global namespace
#include <uchar.h>		// (deprecated) 	behaves as if each name from <cuchar> is placed in global namespace
#include <wchar.h>		// (deprecated) 	behaves as if each name from <cwchar> is placed in global namespace
#include <wctype.h>		// (deprecated) 	behaves as if each name from <cwctype> is placed in global namespace

Unsupported C headers
The C headers #include <stdatomic.h>, <stdnoreturn.h>, and <threads.h> are not included in C++ and have no cxxx equivalents.

Empty C headers
The headers #include <complex.h>, <ccomplex>, <tgmath.h>, and <ctgmath> do not contain any content from the C standard library and instead merely include other headers from the C++ standard library. The use of all these headers is deprecated in C++.
#include <ccomplex>		// (since C++11)(deprecated in C++17) 	simply includes the header <complex>
#include <complex.h>		// (deprecated) 	simply includes the header <complex>
#include <ctgmath>		// (since C++11)(deprecated in C++17) 	simply includes the headers <complex> and <cmath>: the overloads equivalent to the contents of the C header tgmath.h are already provided by those headers
#include <tgmath.h>		// (deprecated) 	behaves as if each name from <ctgmath> is placed in global namespace

Meaningless C headers
The headers #include <ciso646>, <cstdalign>, and <cstdbool> are meaningless in C++ because the macros they provide in C are language keywords in C++.
#include <ciso646>		// 	empty header. The macros that appear in iso646.h in C are keywords in C++
#include <iso646.h>		// (deprecated) 	behaves as if each name from <ciso646> is placed in global namespace
#include <cstdalign>		// (since C++11)(deprecated in C++17) 	defines one compatibility macro constant
#include <stdalign.h>		// (deprecated) 	behaves as if each name from <cstdalign> is placed in global namespace
#include <cstdbool>		// (since C++11)(deprecated in C++17) 	defines one compatibility macro constant
#include <stdbool.h>		// (deprecated) 	behaves as if each name from <cstdbool> is placed in global namespace
************** */




