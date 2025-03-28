#pragma once
#include <string>
// Code below is version 0.3 of project: cpp_output_adapter

namespace Ostream_detail { /// C++20? transform_output_adapter() AND ostream_joiner()

/// transform_output_adapter() adapts an existing output iterator by adding a transformation function which is applied to every output value.
/// how related to: https://en.cppreference.com/w/cpp/iterator/output_iterator

template <class OutputIterator, class Transformer> struct transform_output_adapter {
    OutputIterator out_;
    Transformer    trans_;
public:
    using difference_type   = typename OutputIterator::difference_type; // iterator traits
    using value_type        = typename OutputIterator::value_type;
    using pointer           = typename OutputIterator::pointer;
    using reference         = typename OutputIterator::reference;
    using iterator_category = typename OutputIterator::iterator_category;
    transform_output_adapter( OutputIterator original_iterator, Transformer trans): out_{ original_iterator }, trans_{ trans } {}
    template <class T>
    transform_output_adapter & operator=( const T &value) {
        out_ = trans_(value);
        return *this;
    }
    transform_output_adapter & operator*() { return *this; }
    transform_output_adapter & operator++() { return *this; }
    transform_output_adapter & operator++( int) { return *this; }
};

template<class OutputIterator, class Transformer>
auto make_transform_output_adapter (OutputIterator out, Transformer trans)
    -> transform_output_adapter<OutputIterator, Transformer> {
    return {out, trans};
};

/// ostream_joiner() allows commas between elements, based on std::experimental::ostream_joiner<DelimT,CharT,Traits>::ostream_joiner https://en.cppreference.com/w/cpp/experimental/ostream_joiner
template <class DelimT, class CharT = char, class Traits = std::char_traits<CharT>>
class ostream_joiner {
    using char_type    = CharT;
    using traits_type  = Traits;
    using ostream_type = std::basic_ostream<CharT, Traits>;
    ostream_type &stream_;
    DelimT const  delimiter_;
    bool          first_{ true };
public:
    using difference_type   = void; // iterator traits
    using value_type        = void;
    using pointer           = void;
    using reference         = void;
    using iterator_category = std::output_iterator_tag;

           //using ostream_type = std::basic_ostream<CharT, Traits>;
    ostream_joiner( ostream_type &stream, DelimT const &delimiter) : stream_{ stream }, delimiter_{ delimiter } {}
    ostream_joiner( ostream_type &stream, DelimT      &&delimiter) : stream_{ stream }, delimiter_{ std::move(delimiter) } {}
    ostream_joiner(const ostream_joiner &other) = default;
    ostream_joiner(ostream_joiner &&other)      = default;
    template <class T>
    ostream_joiner &
    operator=( const T &value) {
        if(first_) { first_ = false; } else { stream_ << delimiter_; }
        stream_ << value;
        return *this;
    }
    ostream_joiner &operator=(const ostream_joiner &other) = default;
    ostream_joiner &operator=(ostream_joiner &&other)      = default;
    ostream_joiner &operator*() { return *this; }
    ostream_joiner &operator++() { return *this; }
    ostream_joiner &operator++( int) { return *this; }
};

template <class DelimT, class CharT = char, class Traits = std::char_traits<CharT>>
auto make_ostream_joiner(
    std::basic_ostream<CharT, Traits> &os, DelimT &&delimiter)
    -> ostream_joiner<std::decay_t<DelimT>, CharT, Traits> {
    return { os, delimiter };
};
}
