#include "internal_linkage_component.hpp"
#include "global_entities.hpp"
#include <iostream>

void function1() { // No linkage: local variable, visible only within this function's scope
    int localVar = 1;
    std::cout << "Component1: localVar = " << localVar << std::endl;
}

static int internalVar1 = 10; // Internal linkage: static global variable, visible only within this translation unit

static void internalFunction1() { // Internal linkage: static function, visible only within this translation unit
    std::cout << "Component1: internalFunction1" << std::endl;
}

int externalVar1 = 20; // External linkage: non-static global variable, accessible from other translation units

void externalFunction1() { // External linkage: non-static function, callable from other translation units
    std::cout << "Component1: externalFunction1" << std::endl;
}

// External linkage function demonstrating dual bindage by using component2's external variable
extern int externalVar2; // Declaration of externalVar2 from component2.cpp
void useComponent2() {
    std::cout << "Component1: using externalVar2 = " << externalVar2 << std::endl;
}
