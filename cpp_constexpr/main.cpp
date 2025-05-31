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

    define NDEBUG if asserts are NOT to be checked.  Put in *.h file not *.CPP
    #define NDEBUG
    define GR_DEBUG if we/programmer is Debugging.  Put in *.h file not *.CPP
    #define GR_DEBUG
    #ifdef GR_DEBUG
    #endif GR_DEBUG
 */

#include "global_entities.hpp"
#include <gsl/gsl>      // sudo dnf install  guidelines-support-library-devel
//#include <bits/stdc++.h>
#include <cassert>
#include <chrono>
#include <climits>
#include <csignal>
#include <iostream>
#include <syncstream>
#include <numeric>
#include <optional>
#include <ranges>
#include <source_location>
#include <stacktrace>
#include <stop_token>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>
using std::cin; using std::cout; using std::cerr; using std::clog; using std::endl; using std::string;  // using namespace std;
using namespace std::string_literals;
using namespace std::chrono_literals;

namespace Detail {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
} // End Namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

// ++++++++++++++++ EXAMPLEs begin ++++++++++++++++++++++++++++++++++++++++++++++++++++++++

namespace ExampleP08 { // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
/** Using consteval to make constexpr execute at compile-time C++20.
 The downside of consteval functions is that such functions can’t evaluate at runtime, making them less flexible than constexpr
functions, which can do either. Therefore, it would still be useful to have a convenient way to force constexpr functions to
evaluate at compile-time (even when the return value is being used where a constant expression is not required), so that we could
have compile-time evaluation when possible, and runtime evaluation when we can’t.
 Consteval functions provides a way to make this happen, using a neat helper function:
Uses abbreviated function template (C++20) and `auto` return type to make this function work with any type of value
*Note: not sure what AI or URL this was found on.
We cover auto return types in lesson 10.9 -- Type deduction for functions.
We cover abbreviated function templates (auto parameters) in lesson 11.8 -- Function templates with multiple template types.
*/
consteval auto compileTime (auto value) {
    static_assert(std::is_constant_evaluated());
    return value;
}

constexpr int greater(int x, int y)  {
    static_assert(std::is_constant_evaluated());
    return (x > y ? x : y);
}

void test1 () { std::cout<< "START                ExampleP08 test1. ++++++++++++++++++++++++"<<std::endl;
    std::cout << greater(5, 6) << '\n';              // may or may not execute at compile-time
    std::cout << compileTime(greater(5, 6)) << '\n'; // will execute at compile-time
    int x { 5 };
    std::cout << greater(x, 6) << '\n';              // we can still call the constexpr version at runtime if we wish

    std::cout<< "END                  ExampleP08 test1. ++++++++++++++++++++++++"<<std::endl;
} } // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

namespace ExampleNM1 { // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
// Constexpr/consteval function parameters are not constexpr (but can be used as arguments to other constexpr functions)
constexpr int goo(int c) {
    static_assert(std::is_constant_evaluated());
    return c; }

//constexpr std::pair<int,bool> foo(int b) {
constexpr int foo(int b) {
    //static_assert(std::is_constant_evaluated());
    //assert(std::is_constant_evaluated());
    //constexpr int b2 { b }; // compile error: b is not a constant expression within foo()
    return goo(b);          // okay: b can still be used as argument to constexpr function goo()
    //return { goo(b),std::is_constant_evaluated() };
}

void test1 () { std::cout<< "START                ExampleNM1 test1. ++++++++++++++++++++++++"<<std::endl;
    constexpr int a { 5 };
    int           b { rand() };
    //constexpr auto bar1{ foo(a) };
    //auto bar3 = foo(b);

    //std::cout << "=" << foo << endl; // okay: constant expression a can be used as argument to constexpr function foo()
    //std::cout << "foo(a)=" << bar1.first << "," << bar1.second << endl; // okay: constant expression a can be used as argument to constexpr function foo()
    //std::cout << "foo(b)=" << bar3.first << "," << bar3.second << endl; // okay: constant expression a can be used as argument to constexpr function foo()
    std::cout<< "END                  ExampleNM1 test1. ++++++++++++++++++++++++"<<std::endl;
} } // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

namespace ExampleJonsTake { // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
constexpr auto factorial(int val) {
    if (val == 0) return 1;
    return val * factorial( val - 1);
}

constexpr auto factorial2(int val) {
    if (val == 0) return 1;
    return val * factorial( val - 1);
}

constexpr auto factorial3(int i) {
	int f{1};
    for (int j{2}; j <= i; ++j) { f *= j; }
	return f;
}

// vishal
constexpr auto factorial4(int val){
    auto result{ 1 };
    std::vector<int> v{val+1};
    v[0] = 1;
    std::iota(v.begin(), v.end(), 1);
    //for (int i{1}; i< (val +1); ++i)
    for (auto i : std::ranges::iota_view{ 1, val +1})
        result *= i;
    return result;
}

template <std::size_t...Is>
constexpr auto factorial4_impl(std::index_sequence<Is...>) {
	return (1 * ... * (Is + 1));
}

template <std::size_t I>
constexpr auto factorial4() {
	return factorial4_impl(std::make_index_sequence<I>());
}

constexpr auto call_factorial(int val) {
    auto ice{std::is_constant_evaluated()? 1: -1};
    //int r {rand()};
    std::vector<int> v{1,323,33};
    v.push_back(val);
    v.push_back(ice*val);
    //v.push_back(rand());
    return factorial(val) * ice * v[1];
}

void test1 () { std::cout<< "START                ExampleJonsTake test1. ++++++++++++++++++++++++"<<std::endl;
    //auto myValue{call_factorial(6)};
    constexpr auto myValue{call_factorial(6)};
    //std::array<int, call_factorial(6)> myValues;
    //std::cout << sizeof(myValues) << "\n";
    std::cout << "myValue: " << myValue << "\n";

	static_assert(factorial3(6) == 720);
	static_assert(factorial3(1) == 1);
    static_assert(factorial4<1>() == 1);
	static_assert(factorial4<6>() == 720);
    static_assert(factorial4<10>() == 3628800);

    std::cout<< "END                  ExampleJonsTake test1. ++++++++++++++++++++++++"<<std::endl;
} } // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

namespace ExampleJon202505 {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
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

#define  MY_NUM        43;
const            int a{43};
constexpr        int b{43};
//inline constexpr int c{43};

// constexpr is a suggestion, need not be done by the compiler.
//constexpr int f( int const i) {  // this const only promises to not change within.
consteval int f( int const i) {  // this const only promises to not change within.
    int const     a{i};
    int constexpr b{2};  // known at compile at time and const.
    std::array<int,b> my_array {2,3}; (void)my_array;
    return i+99;
}

constexpr int g( int const i) {
    //int const     a{ f(i) };
    int const     b{ f(2) };
    //int constinit b1{ f(2) };
    int constexpr c{ f(2) };     // known at compile at time and const.
    //int constexpr d{ f(i) };
    int const x    {33};
    int constexpr e{ f( float {33} ) };
    std::array<int,c> my_array {2,3}; (void)my_array;
    return i+99;
}

void test1 () {
    for ( int i{0}; i<5 ;++i)
        //    f(i);
        ;
}
} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

namespace ExampleP10 { // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

/* Constexpr/consteval function parameters are not constexpr (but can be used as arguments to other constexpr functions)
 OBVIOUS: The parameters of a constexpr function are not constexpr (and thus cannot be used in constant expressions). Such parameters can be
declared as const (in which case they are treated as runtime constants), but not constexpr. This is because a constexpr function can
be evaluated at runtime (which wouldn’t be possible if the parameters were compile-time constants).
  However, an exception is made in one case: a constexpr function can pass those parameters as arguments to another constexpr function, and that subsequent
constexpr function can be resolved at compile-time. This allows constexpr functions to still be resolved at compile-time when they
call other constexpr functions (including themselves recursively).
  Perhaps surprisingly, the parameters of a consteval function are
not considered to be constexpr within the function either (even though consteval functions can only be evaluated at compile-time).
This decision was made for the sake of consistency.
*/
constexpr int goo(int c) {
    //static_assert(std::is_constant_evaluated());
    //if (std::is_constant_evaluated()) cout << "WHAT\n";
    //else cout << "Who\n.";
    return c;
}

constexpr int foo(int b) {
    //static_assert(std::is_constant_evaluated());
    //if ( std::is_constant_evaluated() ) cout << "WHAT\n";
 // constexpr int b2 { b }; // compile error: b is not a constant expression within foo()
    return goo(b);          // okay: b can still be used as argument to constexpr function goo()
}

//constinit int e { 5 };
void test1 () { std::cout<< "START                ExampleP10 test1. ++++++++++++++++++++++++"<<std::endl;
    constexpr int a { 5 };
    std::cout << foo(a) << endl; // okay: constant expression a can be used as argument to constexpr function foo()

 // const     int c { 5 };
 // static constinit int d { 5 };  // B constinit int f { 5 };
 // int           b { rand() };
 // std::cout << foo(b) << endl; //
 // std::cout << foo(c) << endl; //
 // std::cout << foo(3) << endl; //
    std::cout<< "END                  ExampleP10 test1. ++++++++++++++++++++++++"<<std::endl;
} } // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

namespace ExampleP12 { // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
/* Can a constexpr function call a non-constexpr function?
    The answer is yes, but only when the constexpr function is being evaluated in a non-constant context. A non-constexpr function may not
    be called when a constexpr function is evaluating in a constant context (because then the constexpr function wouldn’t be able to
    produce a compile-time constant value).
*/
constexpr int someConstexprFcn(){
    return 1212;
}
int someNonConstexprFcn(){
    return 121212;
}
constexpr int someFunction() {
    if (std::is_constant_evaluated()) // if compile-time evaluation
        return someConstexprFcn();    // calculate some value at compile time
    else                              // runtime evaluation
        return someNonConstexprFcn(); // calculate some value at runtime
}

constexpr int someFunction(bool b) {
    if (b)
        return someConstexprFcn();
    else
        return someNonConstexprFcn();
}

void test1 () { std::cout<< "START                ExampleP12 test1. ++++++++++++++++++++++++"<<std::endl;
    int i12 { someFunction(12) };
    int i13 { someFunction(13) };
    std::cout<< "END                  ExampleP12 test1. ++++++++++++++++++++++++"<<std::endl;
} } // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

int main(int argc, char* arv[]) { string my_arv{*arv}; cout << "~~~ argc, argv:"<<argc<<","<<my_arv<<"."<<endl; cin.exceptions( std::istream::failbit);
    Detail::crash_signals_register();
    //if (not std::is_constant_evaluated()) cout << "WHAT\n";  // TODO??: can we output via constexpr?  iostreams is not??
    //ExampleP08::test1 ();
    //ExampleP10::test1 ();
    //ExampleP12::test1 ();
    ExampleJonsTake::test1 ();
    cout << "###" << endl;
    return EXIT_SUCCESS;
}
