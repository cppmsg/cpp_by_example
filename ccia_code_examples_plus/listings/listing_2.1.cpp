#include <thread>

void do_something(int &i) { ++i; }

struct Func {
    int &i;
    Func(int &i_) : i(i_) {}
    void operator()() {
        for(unsigned j = 0; j < 1000000; ++j) {
            do_something(i);
        }
    }
};

void oops() {
    int         some_local_state{0};
    Func        my_func{some_local_state};
    std::thread my_thread{my_func};

    my_thread.detach();
}

int main() {
    oops();
}
