#include <iostream>
using namespace std;

auto my_lambda { [](){} };  // is this static?

int f_in_out( int in_by_copy, int const & in_immutable_by_me_aliasQ, int & out_aliasQ, int & in_out) {
    cout << in_immutable_by_me_aliasQ << endl;
    out_aliasQ=in_by_copy;
    cout << in_immutable_by_me_aliasQ << endl;
    return 1;
};

auto f(int arg) {
    int  a{arg};
    long b{2};
    auto const my_lambda2 { [=](int & c_in_out) mutable {   // [=] by copy [&] by ref
        b=b*a;                                              // b in outer scope will be mutated. no because it will be out of scope.
        return c_in_out*b;
    }};
    b=a*6;
    return my_lambda2;  // returning the closure, removing the const'ness because it logically made a copy, which is NOT const anymore.
};

int main() {
    int c{4};
    int d{5};
    int e{6};
    auto result{ f(33) };
    auto r2{result(c)};
    //cout << r2 << endl;

    //f_in_out(7,c,d,e);
    f_in_out(7,c,c,c);  // two aliases!!
    cout << "###" << endl;
    return 0;
}
