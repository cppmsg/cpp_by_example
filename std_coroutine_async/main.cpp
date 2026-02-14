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

Concurrency alternatives:

boost.fiber
boost.coroutine
boost.context
libmill
libtask
cppcoro
concurrentcpp

GNU Pth
POSIX pthreads
++++++ 1. Boost.Fiber
Boost.Fiber provides a fiber-based concurrency model, which is similar to green threads. It's a lightweight, user-space scheduling library that allows for efficient context switching.
2. POSIX Threads (pthreads)
While not exactly like green threads, pthreads are a popular threading library on Linux. You can use pthread_attr_setstacksize to control stack size and pthread_yield to yield control to other threads.
3. GNU Pth
GNU Pth (Portable Threads) is a user-space threading library that provides a similar functionality to green threads. It's designed to be lightweight and efficient.
4. libcoro
libcoro is a coroutine library that allows for efficient, lightweight context switching. It's designed for high-performance networking and can be used to implement green thread-like behavior.
5. C++20 Coroutines
If you're using a modern C++ compiler, you can leverage C++20 coroutines to implement green thread-like behavior. This is a built-in language feature that provides a lot of flexibility.

++ 1. libco
libco is a lightweight coroutine library that provides a simple, efficient way to implement cooperative multitasking.
2. libtask
libtask is a library that provides a task-based concurrency model, which is similar to green threads. It's designed for high-performance networking and other concurrent applications.
3. State Threads
State Threads is a library that provides a coroutine-based concurrency model. It's designed for high-performance networking and other concurrent applications.
4. ucontext
ucontext is a POSIX API that provides a way to create and manage user-space contexts. You can use it to implement coroutine-like behavior.
5. Boost.Context
Boost.Context provides a library for working with contexts, which can be used to implement coroutine-like behavior.
6. libmill
libmill is a Go-like concurrency library for C that provides a coroutine-based concurrency model.
7. concurrencykit
ConcurrencyKit is a library that provides a variety of concurrency primitives, including coroutines and fibers.
8. seastar
Seastar is a high-performance, coroutine-based framework for building concurrent applications.
9. user-mode threads (UMT)
Some Linux kernels support user-mode threads, which can be used to implement lightweight threading.

+++ In C++ on Linux, there isn't a direct equivalent to Java's green threads (lightweight, user-space threads managed by the JVM). However, you can achieve similar lightweight concurrency using several alternatives. Here are the main options:

 Boost.Fiber:
     A C++ library providing user-space fibers (similar to green threads).
     Fibers are lightweight, cooperative threads managed in user space, with explicit context switching.
     Pros: High performance, fine-grained control, similar semantics to green threads.
     Cons: Requires Boost, not part of standard C++.
     Example:
     cpp

 #include <boost/fiber/all.hpp>
 void task() {
     boost::fibers::fiber([]{ std::cout << "Fiber running\n"; }).join();
 }
 Installation: sudo apt-get install libboost-fiber-dev on Debian/Ubuntu.

C++20 Coroutines:

 Modern C++ (C++20 and later) supports coroutines, which can be used to implement lightweight, cooperative concurrency.
 Coroutines allow suspending and resuming execution, mimicking green thread behavior.
 Pros: Standard C++, no external dependencies, highly flexible.
 Cons: Requires compiler support (e.g., GCC 10+, Clang 14+), steeper learning curve.
 Example (simplified):
 cpp

     #include <coroutine>
     #include <iostream>
     struct Task {
         struct promise_type {
             Task get_return_object() { return {}; }
             std::suspend_never initial_suspend() { return {}; }
             std::suspend_never final_suspend() noexcept { return {}; }
             void return_void() {}
             void unhandled_exception() {}
         };
     };
     Task coro() {
         std::cout << "Coroutine running\n";
         co_return;
     }
     int main() {
         coro();
     }
     Note: Requires a coroutine library (e.g., cppcoro) or custom framework for practical use.
 libuv:
     A cross-platform asynchronous I/O library (used by Node.js) that supports lightweight event loops.
     Not true green threads but enables non-blocking, event-driven programming.
     Pros: Mature, cross-platform, good for I/O-bound tasks.
     Cons: Different model (event loop vs. threads), C-based API.
     Installation: sudo apt-get install libuv1-dev.
 Pthreads with User-space Scheduling:
     Use POSIX threads (pthreads) with a custom user-space scheduler to emulate green threads.
     Pros: Full control, no external libraries beyond standard Linux.
     Cons: Complex to implement, error-prone.
     Example: Requires building a custom scheduler, not recommended unless necessary.
 libgo (Go-like concurrency):
     A C++ library inspired by Go's goroutines, providing lightweight threads with a runtime scheduler.
     Pros: Closer to green threads, high-level API.
     Cons: Less common, may not be as mature.
     Installation: Check GitHub for libgo or similar projects.

Recommendation:

 For most use cases, Boost.Fiber is the closest alternative to Java green threads, offering lightweight, cooperative threading with good performance.
 If you prefer standard C++ and your compiler supports C++20, explore coroutines with a library like cppcoro.
 For I/O-heavy applications, consider libuv for an event-driven approach.


+++++++
















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
void test1 () {
}
} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

namespace Example2 {  // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN
void test1 () {
}
} // END namespace NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

int main(int argc, char const * arv[]) {
    std::string my_arv{*arv}; cout << "$$ ~~~ argc, argv:"<<argc<<","<<my_arv<<"."<<endl;
    cin.exceptions( std::istream::failbit);
    //Detail::crash_signals_register();

    // NO work done yet..
    //Example1::test1 ();
    Example2::test1 ();
    cout << "###" << endl;
    return EXIT_SUCCESS;
}
