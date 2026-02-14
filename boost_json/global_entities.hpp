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

    Ordering of headers as follows: re:Lakos + The prototype/interface header for this implementation (ie, the .h/.hh file that corresponds to this .cpp/.cc file).
    + Other headers from the same project, as needed.
    + Headers from other non-standard, non-system libraries (for example, Qt, Eigen, etc).
    + Headers from other "almost-standard" libraries (for example, Boost)
    + Standard C++ headers (for example, iostream, functional, etc.)
    + Standard C headers (for example, cstdint, dirent.h, etc.)

    As per Anthony Williams C++ on Sea 2022 "intro to Multithreading" @33 min.
    + Order of selecting C++ synchonization facilites:
    1) Latches <== think this first and the proceed down list as required.
    2) Barriers
    3) Futures
    4) Mutexes (binary semaphore)
    5) Semaphores
    6) Atomics (last choice, your profiling has told you that you need it.)
    + We have 6 Mutexes (for most code 5 too many LOL):
    1) std::mutex  <== USE THIS ONE for most, rethink your code. if need then:
    2) std::timed_mutex
    3) std::recursive_mutex
    4) std::recursive_timed_mutex
    5) std::shared_mutex
    6) std::shared_timed_mutex
        + RAII type Locking of those Mutexes:
        1) std::scoped_lock (will lock multiple mutexes (bad sign) <== USE THIS ONE
        2) std::unique_lock (if using a std::condition_variable RARE case)
        3) std::lock_guard
        4) std::shared_lock
    + Condition variables optimize wait'ing.
        1) Eg. {Std::unique_lock lock{m};
                my_condition_variable.wait(lock,[]{return my_optional_data.has_value());}
               {Other thread$ { std::scoped_lock ...; data=make_data();} my_condition_variable.notify_one();
                        TODO??:How does this other thread know about my_condition_variable?}
        2) Note: condition_variable_any
    + Semaphores: READ: The Little Book of Semaphores
        1) Counting_semaphore
    + Std::Atomic< T(trivially copyable && bitwise comparable || unique_ptr || weak_ptr) >:

    define NDEBUG if asserts are NOT to be checked.  Put in *.h file not *.CPP
    #define NDEBUG
    define GR_DEBUG if we/programmer is Debugging.  Put in *.h file not *.CPP
    #define GR_DEBUG
    #ifdef GR_DEBUG
    #endif GR_DEBUG
 */
#pragma once
//#include <gsl/gsl>                                // sudo dnf install  guidelines-support-library-devel
//#include <bits/stdc++.h>
//#include <boost/dynamic_bitset.hpp>
//#include <boost/multiprecision/cpp_int.hpp>
//#include <dlib/numeric_constants.h>
#include <bit>
#include <bitset>
#include <chrono>
#include <iostream>
#include <iterator>
#include <map>
#include <optional>
#include <stacktrace>
#include <source_location>
#include <string>
#include <stacktrace>
#include <vector>
#include <cassert>
#include <climits>
#include <csignal>
#include <cmath>
// NOTE: This file/library is NOT header only, so *.hpp and *.cpp have to be copied into each project that uses it.  TODO??: fix this
using std::cin; using std::cout; using std::cerr; using std::clog; using std::endl; using std::string;  // NOT using namespace std;
using namespace std::string_literals;  // Doesn't cause harm?
using namespace std::chrono_literals;  // Doesn't cause harm?

//static_assert(std::endian::native == std::endian::big,  "Memory is big endian or mixed.");
//static_assert(std::endian::native != std::endian::little && std::endian::native != std::endian::big , "Memory is mixed endian.");
  static_assert(std::endian::native == std::endian::little, "Memory is big endian or mixed.");
 #pragma message("$$ Memory is little endian, not mixed, not big.")

// Define   NDEBUG if asserts are NOT to be checked.  Put in *.h file not *.CPP
//#define 	NDEBUG
// Define   GR_DEBUG if we/programmer is Debugging.  Put in *.h file not *.CPP
//#define  	GR_DEBUG
//#ifdef   	GR_DEBUG
//#endif    GR_DEBUG

static_assert(CHAR_MIN < 0, "Char is signed.");
//static_assert(CHAR_MIN == 0, "Char is unsigned.");
#if CHAR_MIN < 0
#pragma message("$$ Char is signed.")
#else
#pragma message("$$ Char is unsigned.")
#endif

#pragma message("$$ Twos Complement integer math, most common, and C++ standard required since C++20.")

// TODO??: Some NULL ideas to improve C++ uses TODO??: are these relevant or usefull, consider incorporating them into grostig_tools.
using Ostring       = std::optional<std::string>;
using Ochar         = std::optional<char>;
using Ointegral     = std::optional<long>;

//inline constexpr char           CHAR_NULL{'\o{177}'};        // Value is unset/not-set, similar to how a SQL DB shows an unset field as NULL, which is different than zero length or some magic number.  Here we turn it into a magic number and hope for the best.
inline constexpr char           CHAR_NULL{CHAR_MIN};        // Value is unset/not-set, similar to how a SQL DB shows an unset field as NULL, which is different than zero length or some magic number.  Here we turn it into a magic number and hope for the best.
inline constexpr signed char    SCHAR_NULL{SCHAR_MIN};       // Value is unset/not-set, similar to how a SQL DB shows an unset field as NULL, which is different than zero length or some magic number.  Here we turn it into a magic number and hope for the best.
inline constexpr unsigned char  UCHAR_NULL{UCHAR_MAX};       // Value is unset/not-set, similar to how a SQL DB shows an unset field as NULL, which is different than zero length or some magic number.  Here we turn it into a magic number and hope for the best.
inline constexpr std::string    STRING_NULL{"NULL"};    // Value is unset/not-set, similar to how a SQL DB shows an unset field as NULL, which is different than zero length or some magic number.  Here we turn it into a magic number and hope for the best.

// Some crude logging that prints source location, where X prints a variable, and R adds \n\r (which is usefull when tty in in RAW or CBREAK mode. Requires C++20.
#define LOGGER_(  msg )  using loc = std::source_location;std::cout.flush();std::cerr.flush();std::cerr<<    "["<<loc::current().file_name()<<':'<<std::setw(4)<<loc::current().line()<<','<<std::setw(3)<<loc::current().column()<<"]`"<<loc::current().function_name()<<"`:" <<#msg<<           "."    <<endl;cout.flush();cerr.flush();
#define LOGGER_R( msg )  using loc = std::source_location;std::cout.flush();std::cerr.flush();std::cerr<<"\r\n["<<loc::current().file_name()<<':'<<std::setw(4)<<loc::current().line()<<','<<std::setw(3)<<loc::current().column()<<"]`"<<loc::current().function_name()<<"`:" <<#msg<<           ".\r\n"<<endl;cout.flush();cerr.flush();
#define LOGGERX(  msg, x)using loc = std::source_location;std::cout.flush();std::cerr.flush();std::cerr<<    "["<<loc::current().file_name()<<':'<<std::setw(4)<<loc::current().line()<<','<<std::setw(3)<<loc::current().column()<<"]`"<<loc::current().function_name()<<"`:" <<#msg<<".:{"<<x<<"}."    <<endl;cout.flush();cerr.flush();
#define LOGGERXR( msg, x)using loc = std::source_location;std::cout.flush();std::cerr.flush();std::cerr<<"\r\n["<<loc::current().file_name()<<':'<<std::setw(4)<<loc::current().line()<<','<<std::setw(3)<<loc::current().column()<<"]`"<<loc::current().function_name()<<"`:" <<#msg<<".:{"<<x<<"}.\r\n"<<endl;cout.flush();cerr.flush();

namespace Detail {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
std::string source_loc();       // forward declaration  TODO??: Why did I do this? Remember this code is a library, but also runable by main.cpp for testing purposes.
void crash_signals_register();  // forward declaration  TODO??: Why did I do this? Remember this code is a library, but also runable by main.cpp for testing purposes.
} // End Namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

// All code below is: version 0.7 copied from project: cpp_concept_print_container_operator_insert

/* Older simpler stuff to print complex objects
// Another approach to print, from: Josuttis
template <class T>
inline void PRINT_ELEMENTS (const T& coll, string optcstr="") {
    typename T::const_iterator pos;
    std::cout << optcstr;
    for (pos=coll.begin(); pos!=coll.end(); ++pos)
        std::cout << *pos << ' ';
    std::cout << std::endl;
}
// Prints contents of a container such as a vector of int's.
// Insertable Concept used by Templated Function definition.
// Older version is here for the record:
        template<class T> std::ostream & operator<<(std::ostream & out, std::vector<T> const & v) { // utility f() to print vectors
        if ( not v.empty() ) {
            out<<'['; std::copy(v.begin(), v.end(), std::ostream_iterator<T>(out, ", ")); out<<"\b\b]";
        }
            return out;
        }

template<typename First, typename Second, typename Third>  // this forward declaration is definitely required to compile
std::ostream & operator<<( std::ostream & out, std::tuple<First,Second,Third> const & my_tuple);

template<class First, class Second>  // Must precede Streamable
std::ostream &
operator<<( std::ostream & out, std::pair<First,Second> const & my_pair) {
 out << "PAIR_MEMBERS["; out << my_pair.first  <<","<< my_pair.second; out << "]"; return out; }

template<class First, class Second, class Third>   // Must precede Streamable
 // TODO??: make it variadic. TODO??: what about something like this: std::copy(my_pair.begin(), my_pair.end(), std::ostream_iterator< typename Container::value_type >( out, ">,<" ));
std::ostream &
operator<<( std::ostream & out, std::tuple<First,Second,Third> const & my_tuple) {
out << "TUPLE_MEMBERS["; out << std::get<0>(my_tuple) << "," << std::get<1>(my_tuple) << "," << std::get<2>(my_tuple); out << "]"; return out; }  //out << "\b\b\b>]"; out.width(); out << std::setw(0);
*/

/* TODO??: Can/Does anyone want to do this with compile time-recursion?
template <typename ...Ts> // MARC
std::ostream&
operator<<(std::ostream& os, const std::tuple<Ts...>& tup) {  // Ts... parameter-pack-expansion
    tuple_streamer_helper(os, tup, std::make_index_sequence<sizeof...(Ts)>());
    return os;
} */

/* TODO??: need compile-time reflection, 26??
template <typename ...ElementTypes, std::size_t ...Indexs>  // ...ElementTypes is a parameter pack
void struct_comma_insertion_stream_helper(std::ostream& os, const std::tuple<ElementTypes...>& tup, const std::index_sequence<Indexs...>&) {
    (
        ( os << std::get<Indexs>(tup) << ',' )
        , ...
        );  // a compile time "right" fold expression ++17  // TODO??: is it unary or binary?
}

//namespace junk {
template <typename StructElementTypes>
std::ostream& operator<<(std::ostream& os, const StructElementTypes& strct) {  // ElementTypes... is a parameter-pack-expansion
 //const std::index_sequence<1,2,sizeof...(Ts)> is{std::make_index_sequence<sizeof...(Ts)>()};
 //tuple_streamer_helper(os, tup, is);
 //tuple_comma_insertion_stream_helper(os, tup, std::make_index_sequence<sizeof...(Ts)>());
 if constexpr (sizeof...(ElementTypes)) {  // int operatorsizeof...(); // non-overloadable.
     os << "[";
     tuple_comma_insertion_stream_helper(os, tup, std::make_index_sequence<sizeof...(ElementTypes) - 1>());  // all but the last element
     os << std::get<sizeof...(ElementTypes) - 1>(tup);  // last element
     os << "]";
 } else { os << "Empty Tuple!"; }
 return os;
}
*/

template <typename ...ElementTypes, std::size_t ...Indexs>  // ...ElementTypes is a parameter pack
void tuple_comma_insertion_stream_helper(std::ostream& os, const std::tuple<ElementTypes...>& tup, const std::index_sequence<Indexs...>&) {
    (
        ( os << std::get<Indexs>(tup) << ',' )
        , ...
        );  // a compile time "right" fold expression ++17  // TODO??: is it unary or binary?
}

//namespace junk {
template <typename ...ElementTypes>
std::ostream& operator<<(std::ostream& os, const std::tuple<ElementTypes...>& tup) {  // ElementTypes... is a parameter-pack-expansion
    //const std::index_sequence<1,2,sizeof...(Ts)> is{std::make_index_sequence<sizeof...(Ts)>()};
    //tuple_streamer_helper(os, tup, is);
    //tuple_comma_insertion_stream_helper(os, tup, std::make_index_sequence<sizeof...(Ts)>());
    if constexpr (sizeof...(ElementTypes)) {  // int operatorsizeof...(); // non-overloadable.
        os << "[";
        tuple_comma_insertion_stream_helper(os, tup, std::make_index_sequence<sizeof...(ElementTypes) - 1>());  // all but the last element
        os << std::get<sizeof...(ElementTypes) - 1>(tup);  // last element
        os << "]";
    } else { os << "Empty Tuple!"; }
    return os;
}

template <class T>
concept Streamable
    = requires( std::ostream & out_concept_parameter ) {
          { out_concept_parameter << T {} } -> std::convertible_to<std::ostream &>;   // bool concept_function definition()
      };

template <class Container>
concept Streamable_container
    = requires( std::ostream & out ) {
          requires not std::same_as<std::string, Container>;                          // bool concept_requires requirement()
          requires not std::same_as<std::string_view, Container>;
          requires     Streamable<typename Container::value_type>;
          // OLD WORKING STUFF { out << typename Container::value_type {} } -> std::convertible_to<std::ostream & >; // OR just $ { out << typename Container::value_type {} };
      };

template<Streamable_container SC>    // Function Template with typename concept being restricted to SC in this case. Similar to Value template
std::ostream &
operator<<( std::ostream & out, SC const & sc) { //LOGGER_()
    if ( not sc.empty() ) {
        out << "[";                    //out.width(9);  // TODO??: neither work, only space out first element. //out << std::setw(9);  // TODO??: neither work, only space out first element.
        // TODO??: why compile error on?: std::copy(sc.begin(), sc.end(), std::ostream_iterator< typename SC::value_type >( out, ">,<" ));
        for( bool first{true}; auto const &i : sc) { // this works, but why not compile std::copy?
            if(first) { out << "<" << i; first = false; }
            else      { out << ">,<" << i; }
        }
        out << ">]" << " ";             // out.width(); out << std::setw(0);
    } else
        out << "[CONTAINTER IS EMPTY]";
    return out;
}
