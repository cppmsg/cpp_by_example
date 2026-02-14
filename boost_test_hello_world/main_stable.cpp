// cppmsg.com Boost 1.0 license.
// see:
//#define BOOST_TEST_DYN_LINK
//#include <boost/test/unit_test.hpp>
#define BOOST_TEST_MODULE hello_world
#include <boost/test/included/unit_test.hpp> // OR  slower way without linking in boost??
#include <bits/stdc++.h>
using namespace std;
using namespace std::string_literals;

namespace impl {
string hello_world( string const & s) {
    string result {s + " and universes"};
    return result;
}
} // END NamespaceNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

BOOST_AUTO_TEST_CASE( unit_position ) {
    BOOST_CHECK_EQUAL( impl::hello_world("Hello world"), "Hello world and universes."s );
}

BOOST_AUTO_TEST_CASE( second_position ) {
    BOOST_CHECK_EQUAL( impl::hello_world("Hello world"), "Hello world and universes"s );
}

//int main() {
//    //test_();
//    cout << "###" << endl;
//    return EXIT_SUCCESS;
//}
