#pragma once
#include <iostream>
#include <string>
#include <memory>

namespace DualBindage {
class Shape { // Forward Declaration (Interface with Dual Bindage)
public:
    virtual ~Shape() = default;
    virtual std::string draw() const = 0; // Interface function - Externally bound (known at compile time)
};
// Factory function to create Shapes - illustrates runtime determination
std::unique_ptr<Shape> createShape(const std::string& type);
} // namespace DualBindage
