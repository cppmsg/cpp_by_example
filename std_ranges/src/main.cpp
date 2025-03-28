// http://info.gerald-fahrnholz.eu/modern-c/ranges-functional-programming/
// https://learn.microsoft.com/en-us/cpp/standard-library/ranges?view=msvc-170
// https://itnext.io/c-20-ranges-complete-guide-4d26e3511db0
// https://livebook.manning.com/book/functional-programming-in-c-plus-plus/chapter-7/
// https://itnext.io/c-20-ranges-complete-guide-4d26e3511db0
// https://vector-of-bool.github.io/2019/10/21/rngs-static-ovr.html

//#include <bits/stdc++.h>
#include <algorithm>
#include <iostream>
#include <ranges>
#include <string>

#include <typeinfo>
#include <cstdlib>
#include <memory>
#include <cxxabi.h>
using std::endl, std::cin, std::cout, std::cerr, std::string;

auto
demangle( std::type_info const & my_type)->std::string {
    using buffer_t = std::unique_ptr< char, decltype( &std::free)>;
    int result;
    buffer_t buffer{ abi::__cxa_demangle( my_type.name(), nullptr, nullptr, &result), &std::free};
    return 0 == result ? buffer.get() : "Error: in demangling.";
}

template <typename T>
std::string demangle() noexcept {
    return demangle(typeid(T));
}

int main() {  // below code was playing around, so we can just delete it and forget it, or fix it.
    std::string my_string {"abc:hello:my:friend:"};
    //auto range_to_sv { [] (auto && my_range) { return std::string_view( my_range ); }};
    auto range_to_sv { [] (auto my_range) { return std::string_view{ my_range }; }};
    //std::string_view returned = range_to_sv( "xxx");

    //std::ranges::for_each( auto j : split_view ) { }
    for (const auto word : std::ranges::views::split(my_string, ':')) {
        //cout << std::string_view{word} << "," << endl;
        cout << std::string{word.begin(),word.end()} << "," << endl;
    }

    //auto split_view { my_string.std::views::split(':') };
    auto split_view      { my_string | std::views::split(':') };

    // for ( auto i: split_view ) { for ( auto j: i ) { cout << j << "," << endl; } }
    auto drop_view       { split_view | std::views::drop('1') };
    auto transform_view  { drop_view | std::views::transform( range_to_sv ) };
    auto join_view       { transform_view | std::views::join };
    std::ranges::for_each( join_view, [&](const auto& item){ cout << item; });
    cout <<">>"<< *( join_view.begin() ) << endl;
    cout <<">>"<< *( ++(join_view.begin()) ) << endl;
    // cout << join_view << endl;

    /*
       auto range_transform_string {
        my_string
        | std::views::split(':')
        | std::views::drop(1)
        | std::views::transform( range_to_sv )
        | std::views::join
    };
    cout << demangle( typeid( range_to_sv)) << endl;
    cout << demangle( typeid( range_transform_string)) << endl;
    cout << "my_string demangled:" << demangle( typeid(my_string) ) << endl;
    cout << "int demangled:"       << demangle<int>() << endl;
    [[maybe_unused]] int i;
    //cout << "int demangled:"       << demangle(i) << endl;
    //cout << "int demangled:"       << demangle(my_string) << endl;
    std::ranges::for_each( range_transform_string, [](const auto& single_character){ cout << single_character; });
    */

    cout << endl;
    cout << "###" << endl;
    return 0;
}
