/** Copyright (c) Grant Rostig, grantrostig.com 2023. Distributed under the Boost Software License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)
    NOT PRODUCTION QUALITY CODE, just shows learning/teaching example, not real programming, don't copy this style, just playing around
    Reminder of usefull resources:
        https://coliru.stacked-crooked.com/
        https://godbolt.org/
        https://cppinsights.io/
        https://Wandbox.org
        https://cpp.sh/
        https://quick-bench.com/
        https://arnemertz.github.io/online-compilers/
    [[maybe_unused]] int * my_new_var8 = ::new (22,int);
    Language Versions:  clan++ -std=c++2b, g++ -std=c++23 or =gnu++23
    STD Libraries: Gcc: libstdc++; Clang: libc++; Microsoft: msvc/ms stl  // TODO??: how do we link with different libraries than the default using gcc or clang (the exact command line text)?
    g++ -Werror -Weffc++ -Wextra -Wall -Wconversion -Wshadow -Wpedantic -Wold-style-cast -Wsign-promo -Wzero-as-null-pointer-constant -Wsuggest-override -Wnon-virtual-dtor \
        -Wcast-align -Woverloaded-virtual -Wunused -pedantic -Wsign-conversion -Wmisleading-indentation -Wnull-dereference -Wdouble-promotion -Wformat=2 -Wimplicit-fallthrough \
        -Wuseless-cast -Wsuggest-final-types -Wsuggest-final-methods -Wduplicated-cond -Wduplicated-branches -Wlogical-op -std=gnuc++23 \
        main.cpp <OTHER>.cpp -o <A.OUT>

    SYMBOL    MEANING // for debugging purposes
    TODO:     the principal programmer needs todo.
    TODO?:	  the principal programmer is not sure about something, that should be addressed.
    TODO?:X   the X programmer is not sure about something, that should be addressed.
    TODO??:   is a question for verbal discussion at CppMSG.com meetup meetings.

    Ordering of headers as follows: re:Lakos
    + The prototype/interface header for this implementation (ie, the .h/.hh file that corresponds to this .cpp/.cc file).
    + Other headers from the same project, as needed.
    + Headers from other non-standard, non-system libraries (for example, Qt, Eigen, etc).
    + Headers from other "almost-standard" libraries (for example, Boost)
    + Standard C++ headers (for example, iostream, functional, etc.)
    + Standard C headers (for example, cstdint, dirent.h, etc.)

    define NDEBUG if asserts are NOT to be checked.  Put in *.h file not *.CPP
    #define NDEBUG
    define GR_DEBUG if we/programmer is Debugging.  Put in *.h file not *.CPP
    #define GR_DEBUG
    #ifdef GR_DEBUG
    #endif GR_DEBUG
 */

#include "linkage_bindage.hpp" // #include "linkage_bindage_demo.h"

#include "global_entities.hpp"
#include "boost_headers.hpp"
#include "cpp_headers.hpp"
#include "math_grostig.hpp"
#include "ostream_joiner_gr.hpp"
#include <boost/dynamic_bitset.hpp>
#include <boost/multiprecision/cpp_int.hpp>
//#include<dlib/numeric_constants.h>
//#include <gsl/gsl>      // sudo dnf install  guidelines-support-library-devel
//#include <bits/stdc++.h>

#include <bit>
#include <bitset>
#include <cassert>
#include <chrono>
#include <climits>
#include <cmath>
#include <csignal>
#include <iostream>
#include <optional>
#include <source_location>
#include <string>
#include <stacktrace>
#include <vector>
using std::cin; using std::cout; using std::cerr; using std::clog; using std::endl; using std::string;  // using namespace std;
using namespace std::string_literals;
using namespace std::chrono_literals;
namespace Detail {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

int main(int argc, char const * arv[]) { string my_arv{*arv}; cout << "$$ ~~~ argc, argv:"<<argc<<","<<my_arv<<"."<<endl; cin.exceptions( std::istream::failbit); Detail::crash_signals_register();
    // Example1::test1 ();
    // https://claude.ai/chat/d05396ba-05a2-44ab-8bda-0d8df3455558

    std::cout <<
"\n=== NO LINKAGE EXAMPLES ===" << std::endl;
    demonstrate_no_linkage();

    std::cout <<
"\n=== INTERNAL LINKAGE/bindage? EXAMPLES ===" << std::endl;

    std::cout << "Variable in unnamed namespace (internal linkage): "
              << unnamed_namespace_var << std::endl;

    unnamed_namespace_function();

    std::cout << "Static global variable (internal linkage): "
              << global_static_var_IntLinkXX_IntBind << std::endl;

    std::cout << "Constexpr with internal linkage and bindage: "
              << global_static_constexpr_var_XX_IntBind << std::endl;

    std::cout <<
"\n=== EXTERNAL LINKAGE/bindage? EXAMPLES ===" << std::endl;
    std::cout << "External variable (external linkage, external bindage): "
              << global_extern_var_ExtLink_ExtBindXX << std::endl;
    global_fn_ExtLink();

    ClassDefn_ExtLink_IntBindXX example;
    example.fn_ExtLink_XX();
    std::cout << "Static class member (external linkage): "
              << ClassDefn_ExtLink_IntBindXX::static_data_member_ExtLink_XX << std::endl;

    global_inline_fn_ExtLink_IntBindXX();

    std::cout << "External const (external linkage, external bindage): "
              << global_const_bindage_ExtLink_ExtBind << std::endl;

    std::cout << "Constexpr with external linkage, dual bindage: "
              << global_inline_constexpr_var_ExtLink_DualBind << std::endl;

    global_template_fn_ExtLink_IntBindXX<int>(42);

    cout << "###" << endl;
    return EXIT_SUCCESS;
}
/*
https://claude.ai/chat/d05396ba-05a2-44ab-8bda-0d8df3455558

This program demonstrates the three types of linkage (no linkage, internal linkage, and external linkage) and the three types of bindage (internal bindage, external bindage, and dual bindage) in modern C++.

## Key Concepts Illustrated:

### Linkage Types:
1. **No Linkage**: Variables that can only be referred to from their declaration scope
- Local variables, function parameters, block-scoped variables

2. **Internal Linkage**: Names that can be referred to from multiple scopes within a single translation unit
- Variables/functions in unnamed namespaces
- Static variables at namespace scope
- `constexpr` variables (typically)

3. **External Linkage**: Names that can be referred to from different translation units
- Regular namespace-scope declarations
- Functions without `static`
- Class definitions and their members

### Bindage Types (Physical Linkage):
1. **Internal Bindage**: Entity is bound separately in each translation unit
- Inline functions
- Template functions
- `constexpr` variables when not exported

2. **External Bindage**: Entity is bound once at program level
- Regular global variables
- Non-inline functions
- Static class members

3. **Dual Bindage**: Entity can function with either internal or external bindage
- `inline constexpr` variables
- Some header-only designs that work with both models

*/
