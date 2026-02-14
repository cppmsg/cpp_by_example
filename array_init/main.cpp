#include <iostream>
#include <array>
using namespace std;

struct S {
    string  str;
    int     i;
};

std::array<S, 4> s1{{
    {"abc",1},
    {"def",2}
}};

std::array<unsigned char, 20> a1 {0,1,2};
int main() {
    std::array<unsigned char, 20> a2 {0,1,2};
    for (auto & j: s1) {
        cout << j.str << "," << endl;
    }
    return 0;
}
