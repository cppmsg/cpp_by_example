// File: linkage_bindage_demo.h
#pragma once
#include <iostream>
#include <string>

void demonstrate_no_linkage();  // Definition moved to .cpp from .hpp

// *** Internal Linkage Examples
namespace {  // unnamed namespace - all have internal linkage
int unnamed_namespace_var = 1;                      // Internal linkage
void unnamed_namespace_function() {                 // Internal linkage
    std::cout << "Function in unnamed namespace (internal linkage)" << std::endl;
}} // NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN

static int global_static_var_IntLinkXX_IntBind = 2;   // Static entities at namespace scope have internal linkage

static constexpr int global_static_constexpr_var_XX_IntBind = 3;   // internal linkage, internal bindage

// *** External Linkage Examples
// Regular namespace-scope declarations have external linkage
extern int global_extern_var_ExtLink_ExtBindXX;
       int global_var_ExtLink_XX;

void global_fn_ExtLink();               // External linkage

class ClassDefn_ExtLink_IntBindXX {            // Class definition - has external linkage
public:
    static int static_data_member_ExtLink_XX;           // Static member with external linkage
    void fn_ExtLink_XX() const {
        std::cout << "member fn (external linkage)" << std::endl;
    }
};

inline void global_inline_fn_ExtLink_IntBindXX() {
    std::cout << "Inline function (external linkage, typically? internal bindage)" << std::endl;
}

extern const int global_const_bindage_ExtLink_ExtBind;  // External linkage, external bindage

// External linkage with Dual Bindage (header-only design)
inline constexpr int global_inline_constexpr_var_ExtLink_DualBind = 5;  // External linkage, dual bindage

template<typename T>
void global_template_fn_ExtLink_IntBindXX(T value) {        //
    std::cout << "Template - has external linkage but typically internal bindage Template function with value: " << value << std::endl;
}


