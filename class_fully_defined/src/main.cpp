/** A Modern C++ class definition example
    showing all required function members:
    + constructors
    + copy, and move
    + operators - unary and binary

    Copyright (c) Grant Rostig, grantrostig.com 2023. Distributed under the Boost Software License, Version 1.0. (See accompanying
 file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt) NOT PRODUCTION QUALITY CODE, just shows learning/teaching
 example, not real programming, don't copy this style, just playing around Reminder of usefull resources:
        https://coliru.stacked-crooked.com/
        https://godbolt.org/
        https://cppinsights.io/
        https://Wandbox.org
        https://cpp.sh/
        https://quick-bench.com/
        https://arnemertz.github.io/online-compilers/
    [[maybe_unused]] int * my_new_var8 = ::new (22,int);
    Language Versions:  clan++ -std=c++2b, g++ -std=c++23 or =gnu++23
    STD Libraries: Gcc: libstdc++; Clang: libc++; Microsoft: msvc/ms stl  // TODO??: how do we link with different libraries than the
 default using gcc or clang (the exact command line text)?
    g++ -Werror -Weffc++ -Wextra -Wall -Wconversion -Wshadow -Wpedantic -Wold-style-cast -Wsign-promo -Wzero-as-null-pointer-constant
 -Wsuggest-override -Wnon-virtual-dtor \
        -Wcast-align -Woverloaded-virtual -Wunused -pedantic -Wsign-conversion -Wmisleading-indentation -Wnull-dereference
 -Wdouble-promotion -Wformat=2 -Wimplicit-fallthrough \
        -Wuseless-cast -Wsuggest-final-types -Wsuggest-final-methods -Wduplicated-cond -Wduplicated-branches -Wlogical-op
 -std=gnuc++23 \ main.cpp <OTHER>.cpp -o <A.OUT>

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

// #include "global_entities.h"
#include <bits/stdc++.h>
#include <cassert>
//#include <chrono>
#include <climits>
#include <csignal>
#include <gsl/gsl> // sudo dnf install  guidelines-support-library-devel
#include <iostream>
#include <optional>
#include <source_location>
#include <stacktrace>
#include <string>
#include <vector>
using std::cerr;
using std::cin;
using std::clog;
using std::cout;
using std::endl;
using std::string; // using namespace std;
using namespace std::string_literals;
using namespace std::chrono_literals;

using std::cerr;
using std::cin;
using std::clog;
using std::cout;
using std::endl;
using std::string; // using namespace std;
using namespace std::string_literals;
using namespace std::chrono_literals;

namespace Detail {         // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
} // End Namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// ++++++++++++++++ EXAMPLEs begin +++++++++++++++++++++++++++++++++++++++++++++
// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

/* Tips from C++ in a Nutshell:
 * P:135: POD hasn't got: user-defined constructor/destructor, copy(&move??) assignment operator
 * 			   virtual, base classes(meaning no inheritance??)
 * 			   nonstatic: private or protected members, data members that are ref
 * 	   ALSO: nonstatic data members must have a POD type, which are: fundamental, enum (enum class??),
 *           POD class (&struct??) or union, ptr to (or array of POD types).
extern "C" int stat( char * filename, struct stat * st);  // elaborated type specifier example

// template <typename T>
*/

class Base_class {                                          // TODO??: Place using statements in public? Currently private. Opinions on better way??
    using Ptr    = Base_class *;
    using Uptr   = std::unique_ptr<Base_class>;
    using Vi     = std::vector<int>;                        // TODO??: is one ever required to do a typedef instead of using??
    using Vvi    = std::vector<std::vector<int> >;
    using Vvs    = std::vector<std::vector<std::string> >;
    using Uptr_v = std::vector<Uptr>;
    // *** Traits ?? ****
    int                 i_{};
    string              s_{};
    Vi                  vi_{};
    Vvi                 vvi_{};
    Vvs                 vvs_{};
    int const           i_const_{ 99 };  // Const must be inited here or on constructor. TODO??: when can we set it?? what order is respected??
    int mutable         i_mutable_{};    // Even in a const Class definition. For example as a data cache, or memoization.
    int static          i_static_;       // compiler::non-const must be init out of line. todo: what if I don't??
    int const static    i_cstatic_{};
    Ptr 				ptr_{nullptr};
    Uptr 				uptr_{nullptr};
    Uptr_v				uptr_v_{nullptr};  // todo:?? put nulls in all members?
    string static 		s_static_;  		   	// non-const must be init out of line. todo: what if I don't??
    Vvs static 			vvs_static_;  		// non-const must be init out of line.
    Vi static           vi_static_;  		// non-const must be init out of line.
    /*  Now for some sophisticated UML relationships - note a Base_class++ aggregate is not the same as UML aggregation, but
         confusingly similar. https://en.cppreference.com/w/cpp/language/aggregate_initialization
        https://stackoverflow.com/questions/4178175/what-are-aggregates-and-pods-and-how-why-are-they-special
        https://softwareengineering.stackexchange.com/questions/255973/c-association-aggregation-and-composition
        https://stackoverflow.com/questions/885937/what-is-the-difference-between-association-aggregation-and-composition
        https://www.youtube.com/watch?v=B46RqPYhEys
        https://www.learncpp.com/cpp-tutorial/aggregation/ */
    Base_class * 	    composed_member_this_ptr_{nullptr};  	// todo:?? or is this an aggregate?
    std::vector<Base_class *> composed_members_this_ptr_v_{nullptr};  // todo:?? put nulls in all members?
                                            //T 			    t_{};		// todo: for template doesn't compile??
public: // *** Things that MUST be public in general
    // *** Essential Operations Functions?? ***
    // Base_class() {};     			                            // Default contructor  TODO??: Do constructors have an implicit/hidden this? todo:
    Base_class () = default;                                        // TODO??: Does this and similar lines defeat other default member functions?
    // Base_class() = delete;
    // Base_class() = 0;
    Base_class (Base_class const & rhs)          : i_{rhs.i_} { i_ = rhs.i_; };     // Copy constructor  // Must be Ref and const.
    Base_class (Base_class const volatile & rhs) : i_{rhs.i_} { i_ = rhs.i_; };     // Copy constructor  // Must be Ref and const.
    Base_class (Base_class const &&rhs)          : i_{rhs.i_} { i_ = rhs.i_; };     // Move constructor  // Must be value acting as an Rvalue?

    ~Base_class () {};                                                 // Destructor default TODO:?? Are more specific destructors generally used?

    Base_class (int const & i)                  : i_{i} {};                   // Copy constuctor
    Base_class (int const & i, int const & i2)  : i_{i}, i_const_{42} {}      // ??Specialized Copy constuctor for int using initializer list. i_const may be inited here. todo: when can we set it?? what order is respected??
    Base_class (int const && i)                 : i_{i} {};                   // Move constuctor


    // Conversion operators & and * // TODO??: YES they exist.  But how they work again?
    // https://github.com/cppmsg/cpp_by_example/blob/master/andrei_mapping_integral_constants_to_types/main.cpp

    // *** Assignment operators ***
    Base_class &
    operator= (Base_class const &rhs) {
        i_   = rhs.i_;
        s_   = rhs.s_;
        vvi_ = rhs.vvi_;
        vvs_ = rhs.vvs_;
        composed_member_this_ptr_ = rhs.composed_member_this_ptr_;
        return *this;
    }; // Copy assignment
    Base_class &
    operator= (int const & rhs) {
        i_ = rhs;
        return *this;
    }; // copy assignment - partial  // todo:?? return what, check all operators!

    // ***** Binary operators *****
    Base_class & operator+ (Base_class const &rhs){ i_ += rhs.i_;return *this;};  // TODO??
    Base_class & operator- (Base_class const &rhs){ i_ += rhs.i_;return *this;};  // TODO??
    Base_class & operator* (Base_class const &rhs){ i_ += rhs.i_;return *this;};  // TODO??
    Base_class & operator/ (Base_class const &rhs){ i_ += rhs.i_;return *this;};  // TODO??
    Base_class & operator% (Base_class const &rhs){ i_ += rhs.i_;return *this;};  // TODO??

    Base_class operator+= (Base_class const &rhs) { i_ += rhs.i_; return *this; };
    Base_class operator+= (int        const &rhs) { i_ += rhs;    return *this; };

    Base_class operator*= (Base_class const &rhs) { i_ *= rhs.i_; return *this; };
    Base_class operator*= (int        const &rhs) { i_ += rhs.i_; return *this; };

    // ***** Normal  Operators *****

    // ***** Special Operators *****
    Base_class  &operator [] (int const & i)        { return *this; };  // TCPL p550,4
    void         operator () (int const & i) const  {};                 // TODO??: Return type?  What should be const?
    Base_class  *operator -> ()                     { return  this; }; // TCPL p554
    Base_class  &operator *  () const { return *ptr_; };        // TCPL p554

    void *operator new (size_t){};  // TCPL p557
    void *operator new[] (size_t){};
    void operator delete (void *, size_t){};
    void operator delete[] (void *, size_t){};


    // ********** Special Case Increment/Decrement Unary Operators ***** for scalars, pointers and iterators // TCPL p557
    Base_class &operator++ (){};    // Prefix  Unary
 // Base_class  operator++ (){};    // Prefix  Unary TODO??: No Ref, WRONG?
    Base_class &operator-- (){};    // Prefix  Unary
    Base_class  operator++ (int){}; // Postfix Unary - NOTE: int here is a dummy flag only, used to denoate postfix!
    Base_class  operator-- (int){}; // Postfix Unary - NOTE: int here is a dummy flag only, used to denoate postfix!

    // ***** Unary  operators *****
    // Base_class  operator!(Base_class &rhs){};                        // todo:?? pre-fix unary
    void operator!(){}; // todo:?? pre-fix unary
    // ********** Postfix Unary  operators *********
    // ********** Prefix  Unary  operators *********


    Base_class operator&= (Base_class const &rhs){};
    Base_class operator& (Base_class const &rhs){};
    Base_class operator&& (Base_class const &rhs){}; // todo: What is this even?

    bool operator<= (Base_class const &){}; // TODO??: What about spaceship operator?
    bool operator>= (Base_class const &){}; // TODO??: What about spaceship operator?
    bool operator!= (Base_class const &){}; // TODO??: What about spaceship operator?

    size_t size (){}; // todo: add nothrow, final
    void   clear (){};
    void   reset (){};
    void   capacity (){};

    void operator<< (Base_class const &rhs){}; // todo:?? bitshift
    void operator>> (Base_class const &rhs){};
    void operator() (Base_class const &rhs){};

    // *** Setters and getters ***
    // *** Ancillary Member Functions ***
    // *** Ancillary Functions ***
    // *** Ordinary Member Functions ***
    // void   f () {};
    auto f () & {};  // TODO??: what is this? // https://en.cppreference.com/w/cpp/language/function  Reference/Ref qualified ref-qualification member functions/ .
                     // Like non const mem fun can only be called ON objects that were instancied as non const objects (AKA not "const objects".  (no this)
    void f () && {}; // TODO??: what is this? // Ref qualified member functions.  May not be static.

    // struct stat st; 			//todo:?? elaborated type specifier example  eg: struct My_type my_var, where struct is needed.
};

// *** Helper Functions ***  TODO??: do any of these make sense, with regard to TCPL p??? ?
//    void operator <=>( Base_class const &,  Base_class const &) {};
// std::ostream &operator<< (std::ostream &os, Base_class &rhs){ return os; }; // todo:?? make this work for more that <char>, the ostream typedef.
std::basic_ostream<char, std::char_traits<char>> & operator<<( std::basic_ostream<char, std::char_traits<char>> & os,       Base_class &rhs) { return os;};  	// todo:?? more template parameters? std::basic_ostream<char, std::char_traits<char>> & operator<<(
std::ostream& operator<< (std::ostream const & os, Base_class const &rhs){ return os;};    // TCPL p568
std::istream& operator>> (std::istream const & is, Base_class const &rhs){ return os;};

bool operator== (Base_class const &lhs, Base_class const &rhs){ return true; };
bool operator!= (Base_class const &lhs, Base_class const &rhs){ return not (lhs == rhs); };

int operator""_UDL(unsigned long long i) { return 42; };        // TCPL p558
int Base_class::i_static_ = 77;

int main (int argc, char *arv[]) {
    string my_arv{ *arv };
    cout << "~~~ argc, argv:" << argc << "," << my_arv << "." << endl;
    cin.exceptions (std::istream::failbit);
    Detail::crash_signals_register ();
    // struct stat st; // todo:?? elaborated type specifier example  eg: struct My_type my_var, where struct is needed.
    Base_class my_c_var;
    Base_class my_c_var2{ 323 };
    //my_c_var2.i_static_ = 123;

    cout << "###" << endl;
    return EXIT_SUCCESS;
};
