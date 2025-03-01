#include "no_linkage_component.hpp"
// --- no_linkage_example.cpp --- (Illustrates No Linkage)
#include <iostream>

namespace NoLinkageComponent {
void demonstrateNoLinkage() {
    int noLinkageVariable = 5;  // No Linkage Variable (only visible within this function scope)
    std::cout << "No Linkage Variable inside a function: " << noLinkageVariable << std::endl;

           // Lambda Function (No Linkage - scope is where it's defined)
    auto noLinkageLambda = [noLinkageVariable]() {
        std::cout << "Lambda accessing no linkage variable from enclosing scope: " << noLinkageVariable << std::endl;
    };
    noLinkageLambda();

    { // Block Scope Example
        int blockScopedVariable = 10; // No linkage, only in this block
        std::cout << "Block Scoped Variable: " << blockScopedVariable << std::endl;
    }
    // blockScopedVariable is not accessible here - no linkage beyond the block

    class MyClass {  // Class Member (Within a class definition - scope is the class)
    public:
        int memberVariable = 20; // No linkage in the sense of external visibility outside class instances
        void showMember() const {
            std::cout << "Class member variable: " << memberVariable << std::endl;
        }
    };
    MyClass instance;
    instance.showMember();
    std::cout << "Accessing class member directly: " << instance.memberVariable << std::endl; // Access is via instance, not linkage concept directly.
}
} // namespace NoLinkageComponent
