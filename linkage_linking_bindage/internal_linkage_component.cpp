#include "internal_linkage_component.hpp"
#include "global_entities.hpp"

// --- internal_linkage_component.cpp --- (Illustrates Internal Linkage and Internal Bindage)
namespace InternalComponent {
// Internal Linkage Function (only visible within this compilation unit)
static void internalFunction() {
    std::cout << "Internal Function called (Internal Linkage).\n";
}

static int internalVariable = 42; // Internal Linkage Variable (only visible within this compilation unit)

// Internal Bindage Type (Type definition scope is limited, but bindage is more about representation)
// While 'static' for classes isn't common at namespace level like for variables/functions,
// we can consider types defined *within* a cpp file (not in a header) as having a form of
// "internal bindage" because their definition and usage are often more tightly coupled
// within this compilation unit.  However, for clarity, typical classes have external bindage.
// To truly show *internal* bindage in type context, consider implementation hiding techniques.
// For now, we'll illustrate with a simple class in the .cpp file.

class InternalType {  // Defined in .cpp -  Illustrative of potentially more "internal" bindage in context.
public:
    void doInternalWork() const {
        std::cout << "InternalType doing internal work.\n";
        internalFunction(); // Can call internalFunction as it has internal linkage in this unit
        std::cout << "Using internal variable: " << internalVariable << std::endl; // Can use internalVariable
    }
};

void callInternalStuff() { // External Linkage Function to demonstrate usage of internal stuff
    internalFunction(); // OK, within the same compilation unit
    std::cout << "Accessing internal variable from external linked function within same unit: " << internalVariable << std::endl;
    InternalType internalTypeInstance;
    internalTypeInstance.doInternalWork();
}
} // namespace InternalComponent
