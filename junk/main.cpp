#include <iostream>
#include <functional>
using namespace std;
    struct MyClass {
        void member_function(int x) { std::cout << "Value: " << x << '\n'; }
    };

int main() {
    MyClass obj;
    auto bound = std::bind(&MyClass::member_function, &obj, std::placeholders::_1 );
    bound(42); // Outputs: Value: 42
    cout << "Hello World!" << endl;
    return 0;
}
