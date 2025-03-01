#include "external_linkage_component.hpp"
#include <iostream>

//namespace ExternalComponent {
void function2() { // No linkage: local variable, visible only within this function's scope
    int localVar = 2;
    std::cout << "Component2: localVar = " << localVar << std::endl;
}

static int internalVar2 = 30; // Internal linkage: static global variable, visible only within this translation unit

static void internalFunction2() { // Internal linkage: static function, visible only within this translation unit
    std::cout << "Component2: internalFunction2" << std::endl;
}

int externalVar2 = 40; // External linkage: non-static global variable, accessible from other translation units

void externalFunction2() { // External linkage: non-static function, callable from other translation units
    std::cout << "Component2: externalFunction2" << std::endl;
}

// External linkage function demonstrating dual bindage by using component1's external variable
extern int externalVar1; // Declaration of externalVar1 from component1.cpp
void useComponent1() {
    std::cout << "Component2: using externalVar1 = " << externalVar1 << std::endl;
}
//} // namespace ExternalComponent
