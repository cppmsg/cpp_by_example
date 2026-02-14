#include <thread>
void do_something(int &i) { ++i; }

struct Func {
    int &i;

    Func(int &i_) : i(i_) {}

    void operator()() {
        for(unsigned j = 0; j < 1000000; ++j) {
            do_something(i);
            //throw;  // re-throw
            throw(99);
        }
    }
};

void do_something_in_current_thread() {
    throw(99);
}

void f() {
    int         some_local_state{0};
    Func        my_func{some_local_state};
    std::thread t{my_func};
    try {
        do_something_in_current_thread();
    } catch(...) {
        t.join();
        throw;
    }
    t.join();
}

void f2() {
    int         some_local_state2{0};
    Func        my_func{some_local_state2};
    try {
        //std::thread t2{my_func};  // Different exception behaviour - UB OR terminate without blocking?
        std::jthread t2{my_func};   // Could block. Comittee "punted", This try block doesn't catch/protect_against my_func() throwing.
        while(true); //std::this_thread::sleep_for(999);
        do_something_in_current_thread();
    } catch(...) {
        throw;
    }
}

int main() {
    f();
    //f2();
}
