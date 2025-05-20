#pragma once

namespace junk {

class Class_with_elements { // C++26?? see above
    int my_int{ 0 };
    // std::string my_string{};  // TODO??: {} do what here, visavis class initialization?

public:
    Class_with_elements();

    // Class_with_elements operator+( Class_with_elements const & arg) const {
    // Class_with_elements temp{};
    // temp.my_int = this->my_int + arg.my_int;
    // return temp ;
    //};

    // friend Class_with_elements operator=( Class_with_elements const & lhs, Class_with_elements const & rhs);  //
    inline friend Class_with_elements &operator+=(Class_with_elements       &lhs, Class_with_elements const &rhs); // binary operator,
    inline friend Class_with_elements  operator+ (Class_with_elements const &lhs, Class_with_elements const &rhs); // binary operator, is not associative
};

inline Class_with_elements &
operator+=( Class_with_elements &lhs, Class_with_elements const &rhs) {
    lhs.my_int += rhs.my_int;
    return lhs;
}

inline Class_with_elements
operator+( Class_with_elements const &lhs, Class_with_elements const &rhs) {
    Class_with_elements temp{ lhs };
    return temp += rhs;
}

} // namespace junk2
