/* Copyright (c) Grant Rostig, grantrostig.com 2023.
   Distributed under the Boost Software License, Version 1.0. (See accompanying file LICENSE_1_0.txt or copy at https://www.boost.org/LICENSE_1_0.txt)
   NOT PRODUCTION QUALITY CODE, just shows learning/teaching example, not real programming, don't copy this style, just playing around
   Example1: The C++ Standard Library - A Tutorial and Reference
   by Nicolai M. Josuttis, Addison-Wesley, 1999
   © Copyright Nicolai M. Josuttis 1999 */
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
#include <functional>
#include <list>
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
/* The C++ Standard Library - A Tutorial and Reference
   by Nicolai M. Josuttis, Addison-Wesley, 1999
   © Copyright Nicolai M. Josuttis 1999
   License unknown.
   page 303-305. */

/*
template <class T>
inline void PRINT_ELEMENTS (const T& coll, string optcstr="") {
    typename T::const_iterator pos;
    std::cout << optcstr;
    for (pos=coll.begin(); pos!=coll.end(); ++pos)
        std::cout << *pos << ' ';
    std::cout << std::endl;
}

class Nth_fo {    // function object that returns true for the nth call
    int nth;       // call for which to return true
    int count;     // call counter
public:
    Nth_fo(int n) : nth(n), count(0) { cout << "I'm in Nth_fo() constructor now.\n"; }
    bool operator() (int not_used_but_required_dt_remove_if) {
        return ++count == nth;
    }
};

bool Nth_fn(int not_used) {    // function only
    static int nth   {3};       // call for which to return true
    static int count {0};       // call counter
    return ++count == nth;
};

/ ** GCC  // remove_if
 *  @brief Remove elements from a sequence using a predicate.
 *  @ingroup mutating_algorithms
 *  @param  __first  A forward iterator.
 *  @param  __last   A forward iterator.
 *  @param  __pred   A predicate.
 *  @return   An iterator designating the end of the resulting sequence.
 *  *  All elements for which @p __pred returns true are removed from the range
 *  @p [__first,__last).
 *  *  remove_if() is stable, so the relative order of elements that are
 *  not removed is unchanged.
 *  *  Elements between the end of the resulting sequence and @p __last
 *  are still present, but their value is unspecified.
    License unknown. GPL v2?

template<typename _ForwardIterator, typename _Predicate>
_ForwardIterator remove_if1(_ForwardIterator __first, _ForwardIterator __last, _Predicate __pred) {
    __first = std::find_if(__first, __last, __pred);
    if (__first == __last)
        return __first;
    _ForwardIterator next = __first;
    return remove_copy_if( ++next, __last, __first, __pred);
};

template<typename _ForwardIterator, typename _Predicate>
// &__pred not allowed in parameter list?  Can't pass function object by reference? Compiles, but can't be called!??
_ForwardIterator remove_if5(_ForwardIterator __first, _ForwardIterator __last, _Predicate __pred) {
    _Predicate my_pred = __pred; 		// would this help?
    __first = std::find_if(__first, __last,  __pred);  // note the &
    if (__first == __last)
        return __first;
    _ForwardIterator next = __first;
    return remove_copy_if( ++next, __last, __first,  __pred);  // note the &
};

template<typename _ForwardIterator, typename _Predicate>
_ForwardIterator remove_if2(_ForwardIterator __first, _ForwardIterator __last, _Predicate __pred) {
    while ( __first != __last && !__pred( *__first )) // replaces std::find_if() and fixes unexpected behaviour.
        ++__first;
    if (__first == __last)
        return __first;
    _ForwardIterator next = __first;
    return remove_copy_if( ++next, __last, __first, __pred);
};

template<typename _ForwardIterator, typename _Predicate>
_ForwardIterator remove_if3(_ForwardIterator __first, _ForwardIterator __last, _Predicate __pred) {
    while ( __first != __last && !__pred( *__first )) // replaces std::find_if() and fixes unexpected behaviour.
        ++__first;
    if (__first == __last)
        return __first;
    _ForwardIterator next = __first;
    return remove_copy_if( ++next, __last, __first, __pred);
};

void test1() {
    std::list<int> coll0 { 0,1,2,3,4,5,6,7,8,9 }, coll1{coll0}, coll2 {coll0}, coll3 {coll0}, coll4 {coll0}, coll5{coll0};
    PRINT_ELEMENTS( coll1, "orig        : "); cout << "Remove third element, not also the 6th!\n";
    auto pos0 = remove_if(  coll0.begin(), coll0.end(), Nth_fo{3} ); 		// remove third element
    auto pos1 = remove_if1( coll1.begin(), coll1.end(), Nth_fo{3}(999) ); 		// remove third element, try again!
    //auto pos1 = remove_if1( coll1.begin(), coll1.end(), Nth_fo{3}() ); 		// remove third element, try again!
    //auto pos2 = remove_if2( coll2.begin(), coll2.end(), Nth_fo(3) ); 		// remove third element, try again!
    //auto pos3 = remove_if3( coll3.begin(), coll3.end(), Nth_fo(3) ); 		// remove third element, try again!
    //auto pos4 = remove_if4( coll4.begin(), coll4.end(), Nth_fn(0) ); 		// remove third element, try again!
    //auto pos5 = remove_if5( coll5.begin(), coll5.end(), Nth_fn(0) ); 		// remove third element, try again!
    //coll0.erase( pos0, coll0.end() );	// why is this needed? what does remove_if do? why? strange name?
    coll1.erase( pos1, coll1.end() );	// why is this needed? what does remove_if do? why? strange name?
    //coll2.erase( pos2, coll2.end() );
    //coll3.erase( pos3, coll3.end() );
    //coll4.erase( pos4, coll4.end() );
    //coll5.erase( pos5, coll5.end() );
    PRINT_ELEMENTS( coll0, "0,nth removed1: "); PRINT_ELEMENTS( coll1, "1,nth removed1: "); PRINT_ELEMENTS( coll2, "2,nth removed2: "); PRINT_ELEMENTS( coll3, "3,nth removed3: "); //PRINT_ELEMENTS( coll4, "4,nth removed4: ");
}
*/
} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

namespace Example2 {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
void free_function(int x) { std::cout << x << '\n'; }
void print_sum(int x, int y) { std::cout << x + y << '\n'; }

void f1 () { LOGGER_()
    // Free Function Pointer Example
    //void (*func_ptr)(int) = free_function;
    void (*func_ptr)(int) {free_function};
    func_ptr(1); // Callable: invokes free_function

    // Member Function Pointer Example
    struct MyClass {
        void member_function(int x) {
            LOGGER_("my_message");
            std::cout << x << '\n';
        }
    };

  //void (MyClass::*mem_func_ptr)(int) = &MyClass::member_function;  // TODO??: what on earth?
    void (MyClass::*mem_func_ptr)(int) {&MyClass::member_function};  // TODO??: better than above line?
    MyClass obj;
    // Invoke the member function through the pointer TODO??: AI comment correct?
    (obj.*mem_func_ptr)(2); // Callable: invokes member_function on obj

    // 3 Alternatives to above syntax:
    auto mem_func_ptr2 = &MyClass::member_function;
    (obj.*mem_func_ptr2)(3); // Callable: invokes member_function on obj

    using MemberFuncPtr3 = void (MyClass::*)(int);
    MemberFuncPtr3 mem_func_ptr3 = &MyClass::member_function;
    (obj.*mem_func_ptr3)(4); // Callable: invokes member_function on obj

    // Alternatively, if using a pointer to MyClass
    MyClass* ptr = &obj;
    (ptr->*mem_func_ptr)(100); // Outputs: Value: 100

    // Functor
    struct Functor {
        void operator()(int x) const { std::cout << x << '\n'; }
    };
    Functor f;
    f(5); // Callable: invokes Functor::operator()

    auto lambda = [](int x) { std::cout << x << '\n'; };
    //auto lambda = [](int& x) { std::cout << x << '\n'; };
    lambda(6); // Callable: invokes the lambda's operator()
    auto generic_lambda2 = [](auto& obj, int x) { obj.member_function(x); };
    generic_lambda2(obj, 6); // Callable: invokes the lambda's operator()

    std::function<void(int)> func = [](int x) { std::cout << x << '\n'; };
    func(7); // Callable: invokes the stored lambda

    // auto bound3 = std::bind{print_sum, std::placeholders::_1, 10};  // No compile
    auto bound = std::bind( print_sum, std::placeholders::_1, 10);  // TODO??: _1 is what ??
    bound(8,99999999);                                                // Callable: invokes print_sum(8, 10) TODO??: ignores 99999999?


    /*  std::bind with Member Functions:
     Description: The std::bind function (from <functional>, introduced in C++11) can bind a member function to an object, creating a callable that does not require the .* or ->* syntax.

  #include <iostream>
  #include <functional>
  struct MyClass {
      void member_function(int x) { std::cout << "Value: " << x << '\n'; }
  };
  int main() {
      MyClass obj;
      auto bound = std::bind(&MyClass::member_function, &obj, std::placeholders::_1);
      bound(42); // Outputs: Value: 42
      return 0;
  }
  Explanation:
     std::bind binds the member function &MyClass::member_function to the object obj (passed as a pointer)
     and uses std::placeholders::_1 to indicate that the first parameter (x) will be provided when the callable is invoked.
     The resulting bound callable takes a single int argument and invokes obj.member_function(x).
     Advantages:
         Eliminates the need for .* or ->* syntax.
         Can be stored in std::function or used directly.

          Limitations:
              std::bind has some overhead and is considered less idiomatic in modern C++ compared to lambdas (C++14 generic lambdas often replace std::bind).
              The bound object must remain valid during the callable’s lifetime.
    */
    struct MyClass2 {
        // static void member_function1(int x) {  // TODO??: why not a static?
        void member_function1(int x) {
            LOGGER_("my_message");
            std::cout << x << '\n';
        }
        void print_sum(int x, int y) {
            LOGGER_("my_message");
            std::cout << x+y << '\n';
        }
    };
    MyClass2 obj2;
    auto bound2 = std::bind( &MyClass2::member_function1, &obj2, 11);
    auto bound3 = std::bind( &MyClass2::print_sum,        &obj2, std::placeholders::_1, 12);
    bound2(13);                                                // Callable: invokes print_sum(8, 10) TODO??: ignores 99999999?
    bound2();                                                // Callable: invokes print_sum(8, 10) TODO??: ignores 99999999?
    bound3(14,99999999999);                                    // Callable: invokes print_sum(8, 10) TODO??: ignores 99999999?
    LOGGER_()
}

void test1 () { LOGGER_()
    f1();
    LOGGER_()
}
void test2 () {LOGGER_()
    LOGGER_()
}
} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

int main(int argc, char const * arv[]) {
    string my_arv{*arv}; cout << "$$ my_this: argc, argv:"<<argc<<","<<my_arv<<"."<<endl;
    cin.exceptions( std::istream::failbit);
    Detail::crash_signals_register();
// NO RECENT WORK, but had planned to discuss on CppMSG as of: 6/18/2025
    //Example1::test1 ();
    Example2::test1 ();
    cout << "###" << endl;
    return EXIT_SUCCESS;
}
