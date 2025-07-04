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
#include "global_entities.hpp"
//#include "boost_headers.hpp"
//#include "cpp_headers.hpp"
//#include "math_grostig.hpp"
//#include "ostream_joiner_gr.hpp"
//#include "random_toolkit.hpp"
//#include <bits/stdc++.h>
//#include <boost/dynamic_bitset.hpp>
//#include <boost/multiprecision/cpp_int.hpp>
//#include <dlib/numeric_constants.h>
//#include <gsl/gsl>      // sudo dnf install  guidelines-support-library-devel
#include <bit>
#include <bitset>
#include <cassert>
#include <chrono>
#include <climits>
#include <cmath>
#include <csignal>
#include <flat_map>
#include <iostream>
#include <optional>
#include <source_location>
#include <string>
#include <stacktrace>
#include <vector>

using std::cin; using std::cout; using std::cerr; using std::clog; using std::endl; // NOT using namespace std; // duplicated here in case global_entities.hpp is not used.
using namespace std::string_literals;
using namespace std::chrono_literals;

namespace Detail {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
    void f() {LOGGER_()
        LOGGER_()
    }
} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

namespace Example1 {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
    void f1 () { LOGGER_()
        std::vector my_vec{1,2,3};  // If the index is unchecked, it may not get SIGSEGV at(3), or even much higher.
        //for (unsigned int i=0; i<(std::numeric_limits<size_t>::max()-1); ++i) { // TODO??: Why does this give compile warning and all below don't.
        //auto near_max{std::numeric_limits<size_t>::max()-1}; for (unsigned int i=0; i<near_max; ++i) {
        //for (unsigned int i=0; i<(std::numeric_limits<unsigned int>::max()); ++i) {
        //for (size_t i=0; i<(std::numeric_limits<std::size_t>::max()); ++i) {
        //    cout << my_vec[i] <<",";
        //}
        cout << endl;
        LOGGER_()
    }
    void test1 () { LOGGER_()
        int *p{nullptr};
        *p = 42;        // Triggers SIGSEGV
        LOGGER_()
    }
    void test2 () {LOGGER_()
        f1();
        LOGGER_()
    }
} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

int main(int argc, char const * arv[]) {
    string my_arv{*arv}; cout << "$$ my_this: argc, argv:"<<argc<<","<<my_arv<<"."<<endl;
    cin.exceptions( std::istream::failbit);
    Detail::crash_signals_register();

    std::string                 STRING_QQQ          {"qqq"};
    std::vector<char>           VECTOR_CHAR_QQQ     {STRING_QQQ.begin(),STRING_QQQ.end()};

    LOGGER_("testing LOGGER_");                   cout <<"my_endl"<<endl;
    LOGGERX("testing LOGGERX",VECTOR_CHAR_QQQ);   cout <<"my_endl"<<endl;
    cout << VECTOR_CHAR_QQQ <<                           "my_endl"<<endl;
    LOGGER_R("testing LOGGER_R");                 cout <<"my_endl"<<endl;
    LOGGERXR("testing LOGGERXR",VECTOR_CHAR_QQQ); cout <<"my_endl"<<endl;

    //Example1::test2 ();
    //Example1::test1 ();
    cout << "###" << endl;
    return EXIT_SUCCESS;
}
