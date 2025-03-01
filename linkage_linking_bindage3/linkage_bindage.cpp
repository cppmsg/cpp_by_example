// File: linkage_bindage_demo.cpp
#include "linkage_bindage.hpp" //#include "linkage_bindage_demo.h"
#include "global_entities.hpp"
#include <iostream>

/* grostig, move to cpp */
   // No Linkage Examples
void demonstrate_no_linkage() {
    // Local variables have no linkage - only visible within this function
    int local_var = 42;                 // No linkage
    // Block-scoped entities
    {
        int block_scoped = 100;         // No linkage
        std::cout << "Block-scoped variable (no linkage): " << block_scoped << std::endl;
    }
    // Function parameters have no linkage
    auto lambda = [](int param) {       // param has no linkage
        std::cout << "Function parameter (no linkage): " << param << std::endl;
    };
    lambda(local_var);
    std::cout << "Local variable (no linkage): " << local_var << std::endl;
}
/* */

// Definition for global_extern_var_ExtLink declared in header
int global_extern_var_ExtLink = 4;                   // External linkage, external bindage

// Definition for global_const_bindage_ExtLink_ExtBind declared in header
extern const int global_const_bindage_ExtLink_ExtBind = 10;  // External linkage, external bindage

// Definition for static class member
int ClassDefn_ExtLink_IntBindXX::static_data_member_ExtLink_XX = 6;  // External linkage, external bindage

// Implementation for global_fn_ExtLink
void global_fn_ExtLink() {              // External linkage, external bindage
    std::cout << "External function (external linkage, external bindage)" << std::endl;
}
