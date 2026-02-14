#include <iostream>
using namespace std;
//A friend is tutoring using The C++ Primer, 5th Ed. He sent me this: My student pointed out that the Copy-Assignment implementation in section 13.2.2 seems to access memory behind deleted pointers in some cases.  To wit:
//'seems like these assignments could be referencing deleted objects.
class HasPtr {
    int i{};
    int *the_single_use_count_for_shared_ptr{};       // for my c based std::shared_pointer
    int *some_pointer{};    // to anything.
public:
    HasPtr& operator=(const HasPtr &rhs);  // now I have to create: constructor/destructor and everything else...
};
HasPtr& HasPtr::operator=(HasPtr const &rhs) { // parameter is & to avoid copy // binary operator
    ++(*(rhs.the_single_use_count_for_shared_ptr));         // increment the use count of the right hand operand
                                  // NOT: ++(*(this.use_count));         // increment the use count of the right hand operand
  //if (--(*use_count) == 0) {  // then decrement this object's counter
    if (--(*( this->the_single_use_count_for_shared_ptr))== 0) {  // then decrement this object's counter
  //if (--(*(*this).the_single_use_count_for_shared_ptr) == 0) {  // then decrement this object's counter
        delete some_pointer;      // if no other users free this object's allocated members
        delete the_single_use_count_for_shared_ptr;
    }
    some_pointer= rhs.some_pointer;        // copy data (being just a pointer) from rhs into this object
    i           = rhs.i;
    the_single_use_count_for_shared_ptr= rhs.the_single_use_count_for_shared_ptr;
    return *this;
}
int main() {
    HasPtr shared_p_lhs{},shared_p_rhs{};
    shared_p_lhs=shared_p_rhs;
    shared_p_lhs=shared_p_lhs; // a no-op, strange thing to do, copy/assign and then free what is in lhs
    cout << "###" << endl;
    return 0;
}
