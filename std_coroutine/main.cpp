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
//#include "global_entities.hpp"
//#include "boost_headers.hpp"
//#include "cpp_headers.hpp"
//#include "math_grostig.hpp"
//#include "ostream_joiner_gr.hpp"
//#include "random_toolkit.hpp"
//#include <boost/dynamic_bitset.hpp>
//#include <boost/multiprecision/cpp_int.hpp>
//#include<dlib/numeric_constants.h>
//#include <gsl/gsl>      // sudo dnf install  guidelines-support-library-devel
//#include <bits/stdc++.h>

#include <bit>
#include <bitset>
#include <cassert>
#include <chrono>
#include <climits>
#include <cmath>
#include <coroutine>
#include <csignal>
#include <iostream>
#include <syncstream>
#include <optional>
#include <source_location>
#include <string>
#include <stacktrace>
#include <thread>
#include <vector>

using std::cin; using std::cout; using std::cerr; using std::clog; using std::endl; // using namespace std;
using namespace std::string_literals;
using namespace std::chrono_literals;

namespace Detail {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

namespace Example1 {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

void input_thread_internal_f (std::stop_token const &st  ) {
}

void input_thread_f (std::stop_token const &st  ) {
    std::string my_string {"is this temp?"};
    std::string_view my_sview {"is this temp?"};
    cin >> my_string;
    std::osyncstream(cout) << my_string <<"."<< endl;  // TODO??: Is there an isyncstream
}

void output_thread_f (std::stop_token const &st  ) {
    std::osyncstream(cout) << "hello world." << endl;
}

void output_thread_f1 () {
    std::this_thread::sleep_for(std::chrono::seconds {99});
    std::osyncstream(cout) << "hello world1." << endl;
}

void test1 () {
    std::jthread my_input_jthread   {input_thread_f};
    std::jthread my_output_jthread  {output_thread_f};
    my_output_jthread.join();

    std::thread  my_output_thread1 {output_thread_f1};
    my_output_thread1.join();
}

} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

namespace Example2 {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
void input_thread_internal_f (std::stop_token const &st  ) {
}

void input_thread_f (std::stop_token const &my_stop_token, std::stop_source my_stop_source ) {
    std::string my_string{};
    cin >> my_string;
    std::osyncstream(cout) << my_string <<"."<< endl;  // TODO??: Is there an isyncstream
    if ( "q"s == my_string) {
        my_stop_source.request_stop();
    }
}

void output_thread_f (std::stop_token const &st ) {
    std::osyncstream(cout) << "hello world." << endl;
}

void test1 () {
    std::jthread my_output_jthread  {output_thread_f};
    std::jthread my_input_jthread   {input_thread_f, my_output_jthread.get_stop_source()};
}
} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

int main(int argc, char const * arv[]) {
    std::string my_arv{*arv}; cout << "$$ ~~~ argc, argv:"<<argc<<","<<my_arv<<"."<<endl;
    cin.exceptions( std::istream::failbit);
    //Detail::crash_signals_register();

    //Example1::test1 ();
    Example2::test1 ();
    cout << "###" << endl;
    return EXIT_SUCCESS;
}
