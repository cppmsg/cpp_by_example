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
#include "cpp_headers.hpp"
//#include "ostream_joiner_gr.hpp"
//#include <boost/multiprecision/cpp_int.hpp>
//#include<dlib/numeric_constants.h>
//#include <gsl/gsl>      // sudo dnf install  guidelines-support-library-devel
//#include <bits/stdc++.h>
#include <bit>
#include <bitset>
#include <cassert>
#include <chrono>
#include <compare>
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

namespace Strong_ordering_1example {

class Strong_ordering_spaceship {
    int x_{}, y_{};
    friend constexpr std::strong_ordering operator<=>(Strong_ordering_spaceship const& lhs, Strong_ordering_spaceship const& rhs);
    friend           std::ostream&        operator<<( std::ostream& os,              Strong_ordering_spaceship const& s);
public:
    Strong_ordering_spaceship(int x,int y): x_{x}, y_{y} {}
};

constexpr std::strong_ordering operator<=>(Strong_ordering_spaceship const& lhs, Strong_ordering_spaceship const& rhs) {
    if (lhs.x_ < rhs.x_ or (lhs.x_ == rhs.x_ and lhs.y_ < rhs.y_))
        return std::strong_ordering::less;
    if (lhs.x_ > rhs.x_ or (lhs.x_ == rhs.x_ and lhs.y_ > rhs.y_))
        return std::strong_ordering::greater;
    return std::strong_ordering::equivalent;
}

std::ostream& operator<<(std::ostream& os, Strong_ordering_spaceship const& s) {
    return os << '(' << s.x_ << ',' << s.y_ << ')';
}

void print_three_way_comparison(const auto& p, const auto& q) {
    const std::strong_ordering cmp{p <=> q};
    std::cout << p
              << (cmp < 0 ? " <  " : cmp > 0 ? " >  " : " == " ) // compares with 0
              << q << '\n';
}

void print_two_way_comparison(const auto& p, const auto& q) {
    std::cout << p
              << (p < q   ? " <  " : p > q   ? " >  " : " == ") // compares p and q
              << q << '\n';
}

void test1(){
    const Strong_ordering_spaceship p1{0, 1}, p2{0, 1}, p3{0, 2};
    print_three_way_comparison(p1, p2);
    print_two_way_comparison(  p1, p2);
    print_three_way_comparison(p2, p3);
    print_two_way_comparison(  p2, p3);
    print_three_way_comparison(p3, p2);
    print_two_way_comparison(  p3, p2);
}}  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

namespace Weak_ordering_2example {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
class Weak_ordering_spaceship {
    int x_{}, y_{};
    friend constexpr std::strong_ordering operator<=>(Weak_ordering_spaceship const& lhs, Weak_ordering_spaceship const& rhs);
    friend           std::ostream&        operator<<( std::ostream& os,              Weak_ordering_spaceship const& s);
public:
    Weak_ordering_spaceship(int x,int y): x_{x}, y_{y} {}
};

constexpr std::strong_ordering operator<=>(Weak_ordering_spaceship const& lhs, Weak_ordering_spaceship const& rhs) {
    if (lhs.x_ < rhs.x_ or (lhs.x_ == rhs.x_ and lhs.y_ < rhs.y_))
        return std::strong_ordering::less;
    if (lhs.x_ > rhs.x_ or (lhs.x_ == rhs.x_ and lhs.y_ > rhs.y_))
        return std::strong_ordering::greater;
    return std::strong_ordering::equivalent;
}

std::ostream& operator<<(std::ostream& os, Weak_ordering_spaceship const& s) {
    return os << '(' << s.x_ << ',' << s.y_ << ')';
}

void print_three_way_comparison(const auto& p, const auto& q) {
    const std::strong_ordering cmp{p <=> q};
    auto junk {cmp.equal};
    auto junk3{cmp.equal};
    auto junk2{cmp};
    if( cmp == std::strong_ordering::equivalent) {junk;};
    if( cmp.equal == std::strong_ordering::equivalent) {junk2;};
    if( cmp.equal == std::strong_ordering::equal) {junk;};
    if( cmp.equal == std::partial_ordering::unordered) {junk;};
    std::cout << p
              << (cmp < 0 ? " <  " : cmp > 0 ? " >  " : " == " ) // compares with 0
              << q << '\n';
}

void print_two_way_comparison(const auto& p, const auto& q) {
    std::cout << p
              << (p < q   ? " <  " : p > q   ? " >  " : " == ") // compares p and q
              << q << '\n';
}

void test1(){
    const Weak_ordering_spaceship p1{0, 1}, p2{0, 1}, p3{0, 2};
    print_three_way_comparison(p1, p2);
    print_two_way_comparison(  p1, p2);
    print_three_way_comparison(p2, p3);
    print_two_way_comparison(  p2, p3);
    print_three_way_comparison(p3, p2);
    print_two_way_comparison(  p3, p2);
}}

namespace Partial_ordering_3example {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
void test1(){
}}

namespace No_ordering_4example {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
void test1(){
}}

int main(int argc, char const * arv[]) { string my_arv{*arv}; cout << "$$ ~~~ argc, argv:"<<argc<<","<<my_arv<<"."<<endl; cin.exceptions( std::istream::failbit);
    //Detail::crash_signals_register();

    Strong_ordering_1example::test1();
    Weak_ordering_2example::test1();
    Partial_ordering_3example::test1();
    No_ordering_4example::test1();

    cout << "###" << endl;
    return EXIT_SUCCESS;
}
