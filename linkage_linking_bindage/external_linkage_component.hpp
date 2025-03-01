#pragma once
// --- external_linkage_component.h --- (Illustrates External Linkage and External Bindage)

namespace ExternalComponent {
void externalFunction();  // External Linkage Function (visible across compilation units)

// External Linkage Variable (visible across compilation units - use with caution in modern C++)
extern int externalVariable; // Declaration - Definition will be in .cpp

class ExternalType { // External Bindage Type (Defined in header - Type representation is known across compilation units)
public:
    void doExternalWork() const;
};
} // namespace ExternalComponent
