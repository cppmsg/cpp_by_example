// Boost 1.0 license
// Inspired by Jon Kalb's code

// g++ -std=c++2b -Werror -Wpedantic -Wall -Wextra main.t.cpp

#include "biterator.hpp"

#include <algorithm>
#include <iterator>
#include <vector>


int main() {
    /*
        These "tests" are simple demonstrations of how to use the library and require visual
        verification that results are as expected.
        Real unit test coverage needs to be added.
    */

    std::cout<< "\n\n$$ testing biterator::bitset_iter:\n\n";

    std::vector<bool> binary_source{ true, true, false, false, true, true, false, false, true };  // size = 9
    std::string       string_source{ "testing testing" }; //012345678901234
    std::bitset<9>    small_sink{};
    std::bitset<105>   big_sink{};

    std::cout << "$$ binary_source:";
    std::copy(begin(binary_source), end(binary_source), std::ostream_iterator<bool>(std::cout));
    std::copy(begin(binary_source), end(binary_source), biterator::bitset_iter{ small_sink });
    std::cout << "\n$$ small_sink   :" << small_sink;
    std::copy(begin(binary_source) + 3, end(binary_source), biterator::bitset_iter{ small_sink, 3 });
    std::cout << "\n$$ small_sink   :   " << small_sink.to_string().substr(3) << " (offset of 3)\n\n";

    std::cout << "$$ testing biterator::Bitsource_forward_iter:\n\n";

    std::cout << "$$ string_source:";
    std::for_each(begin(string_source), end(string_source),
        [] (char const c)
        { for (int i{6}; i >= 0; --i)
            {
                int mask{1 << i};
                std::cout << ((mask & c)? "1": "0");
            }
        });
    std::copy(biterator::begin<6, 0>(string_source), biterator::end<6, 0>(string_source), biterator::bitset_iter{ big_sink });
    std::cout << "\n$$ big_sink     :" << big_sink;
    auto starting_position{biterator::begin<6, 0>(string_source)};
    std::advance(starting_position, 10);
    std::copy(starting_position, biterator::end<6, 0>(string_source), biterator::bitset_iter{ big_sink, 10 });
    std::cout << "\n$$ big_sink     :          " << big_sink.to_string().substr(10) << " (offset of 10)\n\n";

    std::cout << "$$ testing biterator::Bitout_integral_value:\n\n";

    int32_t int_value{0};
    for (int i{0}; i < 15; ++i)
    {
        std::copy(biterator::bitset_iter{big_sink} + (i * 7), biterator::bitset_iter{big_sink} + ((i + 1) * 7), biterator::begin<int32_t,6,0>(int_value));
        std::cout << int_value << '(' << char(int_value) << ") ";
    }

    std::cout << "\n###" << std::endl;
}
