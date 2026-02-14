/** Copyright (c) Grant Rostig, grantrostig.com 2023.
    Distributed under the Boost Software License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)
    NOT PRODUCTION QUALITY CODE, just shows learning/teaching example, not real programming, don't copy this style, just playing around */
//#include "boost_headers.hpp"
//#include "cpp_headers.hpp"
#include "global_entities.hpp"
//#include "math_grostig.hpp"
//#include "ostream_joiner_gr.hpp"
//#include "random_toolkit.hpp"

//#include <bits/stdc++.h>
//#include <boost/dynamic_bitset.hpp>
//#include <boost/multiprecision/cpp_int.hpp>
//#include <dlib/numeric_constants.h>
//#include <gsl/gsl>      // sudo dnf install  guidelines-support-library-devel
#include <chrono>
#include <iostream>
#include <optional>
#include <string>
#include <vector>

#include <cassert>
#include <climits>
#include <cmath>
#include <csignal>
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
        for (unsigned int i=0; i<(std::numeric_limits<size_t>::max()-1); ++i) { // TODO??: Why does this give compile warning and all below don't.
        //auto near_max{std::numeric_limits<size_t>::max()-1}; for (unsigned int i=0; i<near_max; ++i) {
        //for (unsigned int i=0; i<(std::numeric_limits<unsigned int>::max()); ++i) {
        //for (size_t i=0; i<(std::numeric_limits<std::size_t>::max()); ++i) {
            cout << my_vec[i] <<",";
        }
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

    Example1::test2 ();
    Example1::test1 ();
    cout << "###" << endl;
    return EXIT_SUCCESS;
}
