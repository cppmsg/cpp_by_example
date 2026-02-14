// boost 1.0
#include <memory>
#include <cassert>
#include <iostream>
#include <vector>
#include <experimental/memory>

/* raw ptrs are bad, or even an array?  delete, delete[], free or what?
*/


struct My_type {
    int a{};
};

struct My_derived_type1 : My_type {
    int b{};
};

struct My_derived_type2 : My_type {
    int c{};
};
// unique_ptrs are MOVED.

auto pw1 {std::make_unique<My_type>()}; // maybe implaced

template<class... Ts> // factory function
std::unique_ptr<My_type> make_type(Ts&&... params){
};

int main() {
  //auto pw2{pw1};
    auto pw2{std::move(pw1)};
    auto pw3{std::make_unique<My_type>(*pw2)}; //auto pw4(std::make_unique<My_type>(pw2->));
    make_type<>(); // no leak warning b/c it's smart.
    auto p5{make_type<>()};

    std::vector<int>   v1{2,3};
    std::vector<int> & v2{2,3};









    std::cout << "Hello World!" << std::endl;
    return 0;
}
