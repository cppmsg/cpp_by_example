#include <thread>
void some_function() {}
void some_other_function(int) {}

std::thread f() { // Hardware thread or OS thread or std::thread. The later.
    void some_function();
    return std::thread{some_function};
}
std::thread g() {
    void some_other_function(int);
    std::thread t{some_other_function,42};
    return t;
}

namespace book_example_after_2_5_without_file {
void f_3(std::thread t){
    // destructor of argument.  // terminate ?? on unjoinable thread?
    t.join();  // wait for t to finish.
    // OR  t.detach();  // don't wait for t to finish, but will be OK.
};

void g_3() {
    f_3(std::thread{some_function});  // temp thread constructed, stack_segement/frame of f_3 -> temporary, -> Copy elision to f_3.
    std::thread t(some_function);
    f_3(std::move(t));
}

//void f_const_ref(std::thread const & t){}; //  Futher question
void f_const_ref(std::thread const & t){}; //  Futher question
void g_2() {
    f_const_ref(std::thread(some_function));
    std::thread t{some_function};
    f_const_ref(std::move(t));
}
}

int main() {
    std::thread t1{f()};  // RVO? unnamed?
    t1.join();

    std::thread t2{g()};  // RVO? unnamed?
    t2.join();
}
