#include "dual_bindage_component.hpp"

// --- dual_bindage.cpp --- (Implementation of Dual Bindage, Implementation Bindage is Internal)
namespace DualBindage {
// Concrete Implementation 1 - Circle (Implementation details are internally bound)
class Circle : public Shape {
public:
    std::string draw() const override { return "Drawing a Circle"; }
};

// Concrete Implementation 2 - Square (Implementation details are internally bound)
class Square : public Shape {
public:
    std::string draw() const override { return "Drawing a Square"; }
};

std::unique_ptr<Shape> createShape(const std::string& type) {
    if (type == "circle") {
        return std::make_unique<Circle>();
    } else if (type == "square") {
        return std::make_unique<Square>();
    }
    return nullptr; // Or throw an exception for invalid type
}
} // namespace DualBindage
