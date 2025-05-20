/** NOT PRODUCTION QUALITY CODE, just shows learning/teaching example, not real programming, don't copy this style, just playing around
   Copyright Grant Rostig 2015 - 2023. Distributed under the Boost Software License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)
   Reminder of usefull resources:
   https://coliru.stacked-crooked.com/
   https://godbolt.org/
   https://cppinsights.io/
   https://Wandbox.org
   https://arnemertz.github.io/online-compilers/
    //[[maybe_unused]] int * my_new_var8 = ::new (22,int);
 */

//#include <bits/stdc++.h>
#include "class_with_elements.hpp"

#include <chrono>
#include <csignal>
#include <format>
#include <iostream>
#include <iterator>
#include <map>
#include <ostream>
#include <print>
#include <source_location>
#include <stacktrace>
#include <tuple>
#include <utility>

//import vector;
//import std;

using std::cin; using std::cout; using std::cerr; using std::clog; using std::endl; using std::string;  // using namespace std;
using namespace std::string_literals;
using namespace std::chrono_literals;
template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };

#define LOGGER_(  msg )  using loc = std::source_location;std::cout.flush();std::cerr.flush();std::cerr<<    "["<<loc::current().file_name()<<':'<<std::setw(4)<<loc::current().line()<<','<<std::setw(3)<<loc::current().column()<<"]`"<<loc::current().function_name()<<"`:" <<#msg<<           "."    <<endl;cout.flush();cerr.flush();
#define LOGGER_R( msg )  using loc = std::source_location;std::cout.flush();std::cerr.flush();std::cerr<<"\r\n["<<loc::current().file_name()<<':'<<std::setw(4)<<loc::current().line()<<','<<std::setw(3)<<loc::current().column()<<"]`"<<loc::current().function_name()<<"`:" <<#msg<<           ".\r\n"<<endl;cout.flush();cerr.flush();
#define LOGGERX(  msg, x)using loc = std::source_location;std::cout.flush();std::cerr.flush();std::cerr<<    "["<<loc::current().file_name()<<':'<<std::setw(4)<<loc::current().line()<<','<<std::setw(3)<<loc::current().column()<<"]`"<<loc::current().function_name()<<"`:" <<#msg<<".:{"<<x<<"}."    <<endl;cout.flush();cerr.flush();
#define LOGGERXR( msg, x)using loc = std::source_location;std::cout.flush();std::cerr.flush();std::cerr<<"\r\n["<<loc::current().file_name()<<':'<<std::setw(4)<<loc::current().line()<<','<<std::setw(3)<<loc::current().column()<<"]`"<<loc::current().function_name()<<"`:" <<#msg<<".:{"<<x<<"}.\r\n"<<endl;cout.flush();cerr.flush();

// All code below is: version 0.7 of project: cpp_concept_print_container_operator_insert

/* OLDER simple STUFF
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

// TODO??: Can/Does anyone want to do this with compile time-recursion?
//template <typename ...Ts> // MARC
//std::ostream& operator<<(std::ostream& os, const std::tuple<Ts...>& tup) {  // Ts... parameter-pack-expansion
    //tuple_streamer_helper(os, tup, std::make_index_sequence<sizeof...(Ts)>());
    //return os;
//}

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


// *********************** Tuples  ***************************
template <typename ...ElementTypes, std::size_t ...Indexs>  // ...ElementTypes is a parameter pack
void tuple_comma_insertion_stream_helper(std::ostream& os, const std::tuple<ElementTypes...>& tup, const std::index_sequence<Indexs...>&) {
    (
        ( os << std::get<Indexs>(tup) << ',' )
          , ...
        );  // a compile time "right" fold expression ++17  // TODO??: is it unary or binary?
}

namespace junk {
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
}

// *********************** Containters  ***************************
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
// *********************** END Containters  ***************************

int main ( int argc, char* arv[] ) { string my_arv { *arv}; cout << "~~~ argc,argv:"<<argc<<","<<my_arv<<"."<<endl;
    using My_tuple3 = std::tuple<int,float,std::string>;
    using My_tuple4 = std::tuple<int,float,std::string,std::string>;
    std::vector                         v_int               {1,2,3};
    std::vector                         v_cstring           {"one","two","three"};
    std::vector                         v_string            {"one"s,"two"s,"three"s};
    std::string                         my_string           {"my_chars_string"};
    std::pair<int,float>                my_fundamental_pair {2, 3.3f};
    My_tuple3                           my_tuple3           {2, 3.3f, "threethreethree"s};
    My_tuple4                           my_tuple4           {2, 3.3f, "threethreethree"s, "four"s};
    std::map<int, My_tuple3>::value_type my_value_type      {};
    std::pair                           my_tuple_pair       {my_tuple3,my_tuple3};
    std::vector                         v_fund_pair         {my_fundamental_pair,my_fundamental_pair,my_fundamental_pair};
    std::vector                         v_tuple             {my_tuple3,my_tuple3,my_tuple3};
    std::vector                         v_value_type        {my_value_type, my_value_type, my_value_type};
    std::map<int, My_tuple3>            my_map              { {10,my_tuple3} };
    std::map<int, My_tuple3>            my_map2             { { 10,{11,12.0f,"1st string"s}}, {20,{21,22.0f,"2nd string"s}} };

    //cout << "$$My value_type      :"<< my_value_type << endl;
    //cout << "$$My v_value_type    :"<< v_value_type << endl;
    //cout << "$$My fundamental pair:"<< my_fundamental_pair << endl;        //cout << my_pair.first <<", " << my_pair.second << endl;
// +++++++++++++++++++++++
    //cout << std::get<0>(my_tuple) <<"," << std::get<1>(my_tuple) << "," << std::get<2>(my_tuple) << endl;
    //cout << "$$My tuple           :" << junk::operator<<(cout, my_tuple4) << endl;                  //cout << std::get<0>(my_tuple) <<"," << std::get<1>(my_tuple) << "," << std::get<2>(my_tuple) << endl;
    //cout << "$$My tuple           :" << operator<<(cout, my_tuple4) << endl;                  //cout << std::get<0>(my_tuple) <<"," << std::get<1>(my_tuple) << "," << std::get<2>(my_tuple) << endl;
    //cout << "$$My tuple pair      :"<< my_tuple_pair << endl;
// +++++++++++++++++++++++
    //cout << "$$My v_int           :"<< v_int << endl;
    //cout << "$$My v_cstring       :"<< v_cstring << endl;
    //cout << "$$My v_string        :"<< v_string << endl;
    //cout << "$$My v_fund_pair     :"<< v_fund_pair << endl;  // ERROR
// +++++++++++++++++++++++
    //cout << "$$My map2            :"<< my_map2 << endl;
    //cout << "$$My map             :"<< my_map << endl;

    // *** Marc's tuple solution ***
    //std::index_sequence<4> junk {};  // compile time is: template args, see cppref index_sequence
    //cout << std::index_sequence<4>{} << endl;  // see cppref index_sequence

    //std::cout << std::make_tuple(111) << std::endl;
    //std::cout << std::make_tuple("abc", 2222) << std::endl;
    //std::cout << std::make_tuple(33.33, "def", 333) << std::endl;
    //std::cout << std::make_tuple("four", 4.4, "ghi", 444) << std::endl;
    //std::cout << "$$empty:" << std::make_tuple() << std::endl;

    //using namespace junk;
    //using junk::operator<<;
    //cout << "$$My tuple4          :"<< my_tuple4 << endl;                  //cout << std::get<0>(my_tuple) <<"," << std::get<1>(my_tuple) << "," << std::get<2>(my_tuple) << endl;
    cout << "\n$$My tuple4          :";
    //junk::operator<<(cout, my_tuple4);
    cout << "\n$$My tuple4          :";
    junk::operator<<( cout, my_tuple4);  // binary operator, lhs, rhs
    cout << "\n$$My tuple4 + STUFF:";
    operator<<( junk::operator<<(cout, my_tuple4 ), "STUFF\n");

    // +++++++++++++++++++++++
    //Class_with_elements class_with_elements{60,std::string{"second"}};
    // cout << class_with_elements;  // TODO??: requires reflection??

    // *** Experiment with operators
    // a = b = c;                   // Chaining
    // a.add(b).add(c);             // Chaining
    // a.operator+=(b).add(c);      // TODO??: Does this work? why not? //Chaining

    junk::Class_with_elements c_e1{},c_e2{},c_e3{};
    junk::Class_with_elements const c_e_const1{};
  //c_e1        = c_e1 junk::Class_with_elements::operator+ c_e3;
  //c_e_const1  = c_e1 + c_e1;

    // *** std::print handles printing of containers
    //std::print("v_int:{}. \n", my_string);
    //std::print("v_int:{}.\n", v_int);                          // runs on gcc "truck"
    //std::print("my_tuple3:{}.\n", my_tuple3);      // runs on gcc "truck"
    //std::print("my_map:{}.\n", my_map);                        // ??runs on gcc "truck"

    cout << "###" << endl;
    return EXIT_SUCCESS;
}
