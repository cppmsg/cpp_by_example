// boost 1.0
#include <algorithm>
#include <numeric>
#include <thread>
#include <iostream>
#include <future>
#include <vector>
using namespace std;
namespace detail { double my_task_fn( vector<double>::const_iterator const beg, vector<double>::const_iterator const end, double init) {
    return std::accumulate(beg, end, init);
}} using namespace ::detail;

// future has a promise within (me), both are: a handle to a resource/value (Strustrup), OR promise/future pair (CCIA Williams)
// get_future() from your Packaged_task, then thread the task.
// OR
// store your future which is returned from async'ing a callable.
// THEN
// future.get() your result.

// get futures alternative 1
double concurrent_packaged_task_thread(vector<double> const & v) {
    packaged_task   packaged_task1{my_task_fn}, packaged_task2{my_task_fn};
    future<double>  fut1{ packaged_task1.get_future()}, fut2{ packaged_task2.get_future()};
    auto            first{v.cbegin()};
    auto            v_size{static_cast<std::_Bit_iterator_base::difference_type>(v.size())};
    thread          thread1{ std::move(packaged_task1), first,            first + v_size/2, 0}; // OR use: jthread
    thread          thread2{ std::move(packaged_task2), first + v_size/2, first + v_size,   0};
    double          r1{ fut1.get()}, r2{ fut2.get()};
    thread1.join();
    thread2.detach();  // just showing alternative for example, no functional reason for doing so.
    return r1+r2;
}

// get futures alternative 2
double concurrent_async(vector<double> const & v) {
    auto   first{v.cbegin()};
    auto   v_size{static_cast<std::_Bit_iterator_base::difference_type>(v.size())};
    auto   future1{ async( my_task_fn, first,              first + v_size/4,   0 ) };
    auto   future2{ async( my_task_fn, first + v_size/4,   first + v_size/2,   0 ) };
    auto   future3{ async( my_task_fn, first + v_size/2,   first + v_size*3/4, 0 ) };
    auto   future4{ async( my_task_fn, first + v_size*3/4, first + v_size,     0 ) };
    double r1{ future1.get()}, r2{ future2.get()}, r3{ future3.get()}, r4{ future4.get()};
    return r1+r2+r3+r4;
};

int main() {
    double r1{concurrent_packaged_task_thread( vector{1.5,2.0,3.0,4.0,5.0,6.0} )};
    double r2{concurrent_async(                vector{1.0,2.0,3.0,4.0,5.0,6.0} )};
    cout << r1 << endl;
    cout << r2 << endl;
    cout << "###" << endl;
    return 0;
}
