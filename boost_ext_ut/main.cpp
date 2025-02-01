/** Copyright (c) Grant Rostig, grantrostig.com 2025.
    Derived from https://github.com/boost-ext/ut?tab=readme-ov-file
    Licence Boost 1.0
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
#include <ut.hpp>

#include <algorithm>
#include <array>
#include <bitset>
//#include <gsl/gsl> // sudo dnf install  guidelines-support-library-devel
//#include <bits/stdc++.h>
#include <cassert>
#include <chrono>
#include <climits>
#include <cmath>
#include <csignal>
#include <deque>
#include <expected>
#include <iostream>
#include <map>
#include <numeric>
#include <optional>
#include <set>
#include <source_location>
#include <stacktrace>
#include <string>
#include <unordered_map>
#include <vector>
using std::cin; using std::cout; using std::cerr; using std::clog; using std::endl; using std::string;  // using namespace std;
using namespace std::string_literals;
using namespace std::chrono_literals;
using namespace boost::ext::ut;
using namespace boost::ext::ut::spec;                               // TODO??: did not appear to be needed in examples running on godbolt.  Why?
using namespace boost::ext::ut::bdd;
//using namespace boost::ut::ext::literals;
//using namespace boost::ut::ext::operators;
namespace ut = boost::ext::ut;

// Okay, but my project is more complex than that. How can I scale? Test suites will make that possible. By using suite in
// translation units tests defined inside will be automatically registered. All tests passed (2 asserts in 2 tests)
suite<"my_nameofsuite"> errors = [] { // OR suite errors = []
    "exception"_test = [] { expect(throws([] { throw 0; })) << "throws any exception"; };
    "failure"_test   = [] { expect(aborts([] { assert(false); })); };
}; // int main() { }

int main(int argc, char* arv[]) { string my_arv{*arv}; cout << "$$ ~~~ argc, argv:"<<argc<<","<<my_arv<<"."<<endl; cin.exceptions( std::istream::failbit); // Detail::crash_signals_register();
    //boost::ut::expect(true);
    //boost::ut::expect(1 == 2);
    //boost::ut::expect(1_i == 2);
    //boost::ut::expect(1 == 2_i);
    //boost::ut::expect(1_i == 2_i);
    //boost::ut::expect(that % 1 == 2);
    //boost::ut::expect(that % 1 == % 2); // no compile
    //boost::ut::expect(that % 1 == that % 2); // compile, but doesn't fail!
    //boost::ut::expect( fatal(that % 1 == 2));
    //expect( eq(1, 2)) << "grostig";
    //expect( fatal(eq(1, 2))) << "grostig";
    //expect( 42l == 42_l and 1 == 2_i) << "grostig" << std::endl;  // TODO??: endl does not work.
    //expect( fatal( 42l == 42_l and 1 == 2_i) )<< "grostig. \n";  //
    expect(true);
    expect(                 1   == 2  );
    expect(                 1_i == 2  );
    expect(                 1   == 2_i);
    expect(                 1_i == 2_i);
    expect(                 42l == 42_l and 1 == 2_i );
    expect(         that %  1 == 2);
  //expect(         that %  1 == % 2);                                                      // No compile
    expect(         that %  1 == that % 2);                                                 // TODO??: Compile, but doesn't fail!
    expect(         eq(     1, 2)) << "grostig";
  //expect( fatal(  eq(     42l ,  42_l) and eq( 1 ,  2_i) ));
  //expect(         eq(     42l ,  42_l) and eq( 1 ,  2_i) ) << "$$ grostig.\n"  << fatal;
    expect(         eq(     42l ,  42_l) and eq( 1 ,  2_i) ) << "grostig.\n";               // TODO??: endl does not work.
  //expect(         eq(     42l ,  42_l) and eq( 1 ,  2_i) ) << "grostig" << std::endl;     // TODO??: std::endl does not compile.
  //expect(         eq(     42l ,  42_l) and eq( 1 ,  2_i) ) << "grostig" <<      endl;     // TODO??: just endl does not compile.
  //expect( fatal(          42l == 42_l and 1 == 2_i) )      << "grostig.\n";               // "grostig" doesn't print due to "fatal(" ?? .

    // I use std::expected, can I stream its error() upon failure? Yes, since std::expected's error() can only be called when there is no value it requires lazy evaluation.
    test("lazy log") = [] {  // OR "lazy log"_test = [] {
      std::expected<bool, std::string> e{std::unexpected("lazy evaluted.")};  // TODO??: does "=" change anything? std::expected<bool, std::string> e2 = std::unexpected("lazy evaluted.");
      expect(e.value()) << [&] {return e.error();} << fatal;
      expect(e.value() == true);
    };

    //Let's make our first end-2-end test case, shall we?
    "hello world"_test = [] {
        int i = 43;
        expect(42_i == i);
    };

    // I'd like to nest my tests, though and share setup/tear-down. With lambdas used to represents tests/sections we can easily achieve that. Let's just take a look at the following example. All tests passed (4 asserts in 1 tests)
    "[vector1]"_test = [] {
        std::vector<int> v(5);
        ut::expect(fatal(5_ul == std::size(v)));

        ut::should("resize bigger") = [v] { // or "resize bigger"_test
            mut(v).resize(10);
            expect(10_ul == std::size(v));
        };
        ut::expect(fatal(5_ul == std::size(v)));

        ut::should("resize smaller") = [=]() mutable { // or "resize smaller"_test
            v.resize(0);
            ut::expect(0_ul == std::size(v));
        };
    };

    // Nice! That was easy, but I'm a believer into Behaviour Driven Development (BDD). Is there a support for that? Yes! Same example as above just with the BDD syntax. All tests passed (2 asserts in 1 tests)
    //using namespace boost::ut::bdd;
    "vector2"_test = [] {
        ut::bdd::given("I have a vector") = [] {
            std::vector<int> v(5);
            expect(fatal(5_ul == std::size(v)));

            ut::bdd::when("I resize bigger") = [=] {
                mut(v).resize(10);                  // TODO?: what is mut doing here? supposed to be before the "{" as in vector1_test?

                ut::bdd::then("The size should increase") = [=] { expect(10_ul == std::size(v)); };
            };
        };
    };

    //On top of that, feature/scenario aliases can be leveraged. All tests passed (2 asserts in 1 tests)
    feature("vector") = [] {
        scenario("size") = [] {
            given("I have a vector") = [] {
                std::vector<int> v(5);
                expect(fatal(5_ul == std::size(v)));

                when("I resize bigger") = [=] {
                    mut(v).resize(10);

                    then("The size should increase") = [=] { expect(10_ul == std::size(v)); };
                };
            };
        };
    };

    //Can I use Gherkin? Yeah, let's rewrite the example using Gherkin specification. All tests passed (2 asserts in 1 tests)
    bdd::gherkin::steps steps = [](auto &steps) {
        steps.feature("Vector") = [&] {
            steps.scenario("*") = [&] {
                steps.given("I have a vector") = [&] {
                    std::vector<int> v(5);
                    expect(fatal(5_ul == std::size(v)));

                    steps.when("I resize bigger") = [&] { v.resize(10); };

                    steps.then("The size should increase") = [&] { expect(10_ul == std::size(v)); };
                };
            };
        };
    };
    "Vector"_test = steps | R"(
      Feature: Vector
        Scenario: Resize
          Given I have a vector
           When I resize bigger
           Then The size should increase
    )";

    // Nice, is Spec notation supported as well? All tests passed (2 asserts in 1 tests)
    using namespace boost::ext::ut::spec;
    describe("vector") = [] {
        std::vector<int> v(5);
        expect(fatal(5_ul == std::size(v)));

        it("should resize bigger") = [v] {
            mut(v).resize(10);
            expect(10_ul == std::size(v));
        };
    };

    // That's it! Alternatively, a convenient test syntax is also provided. All tests passed (3 asserts in 3 tests)
    "args"_test = [](const auto &arg) { expect(arg > 0_i) << "all values greater than 0"; } | std::vector{ 1, 2, 3 };

    cout << "###" << endl;
    return EXIT_SUCCESS;
}
