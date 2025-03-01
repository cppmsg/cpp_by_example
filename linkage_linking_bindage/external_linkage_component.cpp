#include "external_linkage_component.hpp"
// --- external_linkage_component.cpp --- (Implementation of External Linkage Component)

namespace ExternalComponent {
// Definition of externalVariable (must be in *one* .cpp file if declared 'extern')
int externalVariable = 100;

void externalFunction() {
    std::cout << "External Function called (External Linkage).\n";
    std::cout << "Accessing external variable: " << externalVariable << std::endl;
}

void ExternalType::doExternalWork() const {
    std::cout << "ExternalType doing external work.\n";
    externalFunction(); // Can call externalFunction as it has external linkage
    std::cout << "Using external variable in member function: " << externalVariable << std::endl;
}
} // namespace ExternalComponent
