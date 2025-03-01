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

#include "external_linkage_component.hpp"
#include "dual_bindage_component.hpp"

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

// Declarations of entities with external linkage from component1.cpp and component2.cpp
extern int externalVar1;
extern int externalVar2;
extern void function1();
extern void function2();
extern void externalFunction1();
extern void externalFunction2();
extern void useComponent2(); // From component1.cpp
extern void useComponent1(); // From component2.cpp

int main(int argc, char const * arv[]) { string my_arv{*arv}; cout << "$$ ~~~ argc, argv:"<<argc<<","<<my_arv<<"."<<endl; cin.exceptions( std::istream::failbit); Detail::crash_signals_register();
    // Example1::test1 ();

    // Demonstrate no linkage: local variables within functions
    std::cout << "=== No Linkage ===\n";
    function1(); // Calls function1, accessing its localVar
    function2(); // Calls function2, accessing its localVar

    // Demonstrate external linkage: accessing variables and calling functions across translation units
    std::cout << "\n=== External Linkage ===\n";
    std::cout << "Main: externalVar1 = " << externalVar1 << std::endl;
    std::cout << "Main: externalVar2 = " << externalVar2 << std::endl;
    externalFunction1();
    externalFunction2();

    // Demonstrate dual bindage: mutual dependencies between components
    std::cout << "\n=== Dual Bindage ===\n";
    useComponent2(); // Component1 uses component2's externalVar2
    useComponent1(); // Component2 uses component1's externalVar1

    // Note: internalVar1, internalVar2, internalFunction1, and internalFunction2
    // have internal linkage and are not accessible here. Attempting to access them
    // (e.g., 'extern int internalVar1;') would result in a linker error.

    cout << "###" << endl;
    return EXIT_SUCCESS;
}
/*
   Explanation of Concepts:
*** Linkage

No Linkage:
    Represented by local variables like localVar in function1() and function2().
    These variables are confined to their function’s scope and invisible outside, demonstrating no linkage across any boundaries.
Internal Linkage:
    Represented by static entities such as internalVar1 and internalFunction1() in component1.cpp, and internalVar2 and
    internalFunction2() in component2.cpp. These are visible only within their respective translation units, preventing access from
    main.cpp or the other component.
External Linkage: Represented by non-static global variables (externalVar1, externalVar2) and
    functions (externalFunction1(), externalFunction2(), useComponent2(), useComponent1()). These are accessible across translation units
    when declared with extern, enabling interaction between main.cpp, component1.cpp, and component2.cpp.

*** Bindage (Physical Linkage)
Internal Bindage:
    Illustrated by the use of internal linkage (static variables and functions) within each component.
    For example, internalVar1 and internalFunction1() in component1.cpp are confined to that file, meaning the component’s
    implementation is self-contained with no external physical dependencies for these entities. This aligns with Lakos’s emphasis on
    minimizing dependencies outside a physical unit.
External Bindage: Illustrated by entities with external linkage (externalVar1,
    externalVar2, etc.). These components expose interfaces that other translation units (main.cpp or the other component) can link
    against, creating physical dependencies. For instance, main.cpp depends on both component1.cpp and component2.cpp to resolve
    externalVar1 and externalVar2.
Dual Bindage: Illustrated by the mutual dependency between component1.cpp and component2.cpp. In
    component1.cpp, useComponent2() accesses externalVar2 from component2.cpp. Conversely, in component2.cpp, useComponent1() accesses
    externalVar1 from component1.cpp. This creates a cyclic physical dependency, where each component requires the other to be linked,
    exemplifying dual bindage. In Lakos’s terms, such cycles are often discouraged in large-scale design, but here they serve to
    demonstrate the concept.

*** Relating Linkage to Bindage

    No Linkage and Internal Bindage: Entities with no linkage (local variables) inherently contribute to internal bindage by keeping
dependencies within their immediate scope, though they operate at a finer granularity than translation units. Internal Linkage and
Internal Bindage: Using static confines symbols to a single translation unit, supporting internal bindage by limiting physical
dependencies to within the component. External Linkage and External/Dual Bindage: External linkage enables external bindage by
exposing symbols for other units to use, and when mutual (as with useComponent1() and useComponent2()), it leads to dual bindage due
to cyclic dependencies.
https://grok.com/chat/c12413b8-9d7b-4e01-8904-d808f94ed336

*/
