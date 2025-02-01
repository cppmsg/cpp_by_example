// Boost 1.0 license
// Inspired by Jon Kalb's code
// Allows manipulation of bits in a bitset, beyond what is supplied in std::  Could be useful in brainwallet project.

#include <bitset>
#include <cassert>
#include <iostream>

namespace biterator {
    namespace detail
    {
            // Used to determine if the bits being read by the Bitsource_forward_iter are ascending or descending.
        enum struct Direction { Up, Down, Neither};
        consteval auto direction(auto start_bit = 0, auto last_bit = 7)
            requires std::is_same_v<decltype(start_bit),decltype(last_bit)> and
            std::is_integral_v<decltype(start_bit)> and std::is_integral_v<decltype(last_bit)>
        {
            if (last_bit == start_bit) return Direction::Neither;
            if (last_bit > start_bit) return Direction::Up;
            return Direction::Down;
        }
        consteval auto make_mask(auto bit_position)
            requires std::is_integral_v<decltype(bit_position)>
        {
            uintmax_t mask_value{ 1 };
            return mask_value << bit_position;
        }
        constexpr auto increment_mask(uintmax_t &mask, Direction d)
        {
            if (d == Direction::Up) { mask <<= 1; } else if (d == Direction::Down) { mask >>= 1; }
            // Direction::neither is a no-op.
        }    
    } // namespace detail

    template <typename Container> concept Integral_container = std::integral<typename Container::value_type>;
    
    /*
        bitset_iter is an random-access iterator that reads/writes a bit at time.
        Note that it starts with the most significant bit and reads to the least significant bit.
    */
    template <std::size_t bitset_size>
    struct bitset_iter
    {
        /*
            The BitRefProxy type is returned when we need to return a reference to a bool inside the bitset.
            Since the bitset is (likely) implemented with bits of integers rather than bools, we can't actually
            return a reference to a bool. Instead we return this class. We can read and write the correct bool
            value this this proxy to the Boolean value in the bitset. 
        */
        struct BitRefProxy
        {
            BitRefProxy() {}
            BitRefProxy(BitRefProxy const&) = default;
            BitRefProxy(std::bitset<bitset_size> & bs, std::size_t initial_offset = 0):
                bs_{ &bs },
                offset_{ bitset_size - initial_offset - 1}
            {}
            operator bool() const {return (*bs_)[offset_];}
            BitRefProxy& operator=(bool v) {(*bs_)[offset_] = v; return *this;}
            private:
                std::bitset<bitset_size> *bs_{nullptr};
                std::size_t              offset_{0};
        };

        // Boiler plate iterator nested types.
        using iterator_category = std::random_access_iterator_tag;
        using difference_type   = std::ptrdiff_t;
        using value_type        = bool;
        using reference         = BitRefProxy;
        // using pointer           = bool *; // Not needed and problematic to implement due to the proxy issue.

        bitset_iter() = default;
        bitset_iter(bitset_iter const& orginal) = default;
        bitset_iter(std::bitset<bitset_size> & bs, std::size_t initial_offset = 0):
            bs_{ &bs },
            offset_{ bitset_size - initial_offset - 1}
        {}
        auto operator*() -> BitRefProxy { return BitRefProxy(*bs_, offset_);}

        // Should probably add some asserts to inc and dec operators.
        auto operator++()    /* prefix */  -> bitset_iter & { --offset_; return *this; }
        auto operator++(int) /* postfix */ -> bitset_iter   { auto result{*this}; ++this; return result; }
        auto operator--()    /* prefix */  -> bitset_iter & { ++offset_; return *this; }
        auto operator--(int) /* postfix */ -> bitset_iter   { auto result{*this}; --this; return result; }

        auto operator==(const bitset_iter& rhs) const
        {
            // "end" iterators are all equal
            if ((not bs_ and not rhs.bs_)
                or (not bs_ and rhs.offset_ + 1 == bitset_size)
                or (offset_ + 1 == bitset_size and not rhs.bs_))
            {
                return true;
            }
            return bs_ == rhs.bs_ and offset_ == rhs.offset_;
        }
        auto operator!=(const bitset_iter& rhs) const { return not (*this == rhs); }
        auto operator=(bool val) -> bitset_iter &
        {
            (*bs_)[offset_] = val;
            return *this;
        }

        // Random Access
        // Comparison functions iterators must reference the same container.
        friend auto operator<(bitset_iter const& rhs,  bitset_iter const& lhs)
        {
            assert(((not rhs.bs_) or (not lhs.bs_)) && "Comparing iterator not referencing a container.");
            assert(rhs.bs_ == lhs.bs_ && "Comparing iterators from different containers.");
            return rhs.offset > lhs.offset; // Offset progress downward, so > offset reflects a < interator.
        };
        friend auto operator>(bitset_iter const& rhs,  bitset_iter const& lhs)
        {
            assert(((not rhs.bs_) or (not lhs.bs_)) && "Comparing iterator not referencing a container.");
            assert(rhs.bs_ == lhs.bs_ && "Comparing iterators from different containers.");
            return rhs.offset < lhs.offset; // Offset progress downward, so < offset reflects a > interator.
        };
        friend auto operator<=(bitset_iter const& rhs,  bitset_iter const& lhs)
        {
            assert(((not rhs.bs_) or (not lhs.bs_)) && "Comparing iterator not referencing a container.");
            assert(rhs.bs_ == lhs.bs_ && "Comparing iterators from different containers.");
            return rhs.offset >= lhs.offset; // Offset progress downward, so >= offset reflects a <= interator.
        };
        friend auto operator>=(bitset_iter const& rhs,  bitset_iter const& lhs)
        {
            assert(((not rhs.bs_) or (not lhs.bs_)) && "Comparing iterator not referencing a container.");
            assert(rhs.bs_ == lhs.bs_ && "Comparing iterators from different containers.");
            return rhs.offset <= lhs.offset; // Offset progress downward, so <= offset reflects a >= interator.
        };
    
        bitset_iter& operator+=(int delta)
        {
            assert(delta <= offset_ && "Incrementing iterator past end of bitset.");
            offset_ -= delta; return this;
        }
        friend bitset_iter operator+(bitset_iter const& lhs, int delta)
        {
            assert(delta <= (lhs.offset_ + 1) && "Incrementing iterator past end of bitset.");
            return bitset_iter(*lhs.bs_, bitset_size - lhs.offset_ - 1 + delta);
        }
        friend bitset_iter operator+(int delta, bitset_iter const& rhs)
        {
            assert(delta <= rhs.offset_ && "Incrementing iterator past end of bitset.");
            return bitset_iter(*rhs.bs_, rhs.offset_ - delta);
        }
        bitset_iter& operator-=(int delta)
        {
            assert(bitset_size - offset_ <= delta && "Decrementing iterator before beginning of bitset.");
            offset_ += delta; return this;
        }  
        friend bitset_iter operator-(bitset_iter const& lhs, int delta)
        {
            assert(bitset_size - lhs.offset_ <= delta && "Decrementing iterator before beginning of bitset.");
            return bitset_iter(*lhs.bs_, lhs.offset_ + delta);
        }  
        friend difference_type operator-(bitset_iter const& lhs, bitset_iter const& rhs)
        {
            assert(rhs.bs_ == lhs.bs_ && "Subtracting iterators from different containers.");
            return difference_type(rhs.offset) - difference_type(lhs.offset_);
            // Offset progress downward, so Subtrahend - Minuend instead of Minuend - Subtrahend.
        }  
    
        reference operator[](std::size_t) const {current_value_ = (*bs_)[offset_]; return current_value_;}
    private:
        std::bitset<bitset_size> *bs_{nullptr};
        std::size_t              offset_{0};
        bool                     current_value_{false};
    };
    
    /*
        Bitout_integral_value is an output iterator that writes bits into an integral values.
        The caller specifies a range of bits with the integral value.
    */
    
    template <auto start_bit, auto last_bit, std::integral integral_t>
        requires std::is_integral_v<decltype(start_bit)> and std::is_integral_v<decltype(last_bit)>
    struct Bitout_integral_value
    {
        // Boiler plate iterator nested types.
        using iterator_category = std::output_iterator_tag;
        using value_type        = bool;
        using difference_type   = std::ptrdiff_t;
        using reference         = bool const &;
        //using pointer           = bool const *;   // probably not needed

        Bitout_integral_value(integral_t& sink): sink_{sink} {}

        auto operator*() -> Bitout_integral_value & { return *this; }
        auto operator++() /* prefix */
        {
            increment_mask(current_mask_, direction_);
            return *this;
        }
        auto operator++(int) /* postfix */
        {
            auto result{*this};
            ++this;
            return result;
        }
        auto operator=(bool val) -> Bitout_integral_value &
        {
            sink_ |= (val * current_mask_);
            return *this;
        }
        auto operator==(Bitout_integral_value const &other) const
        {
            return &sink_ == other.sink_ and current_mask_ == other.current_mask_;
        }
        auto operator!=(Bitout_integral_value const &other) const { return !(*this == other); }

    private:
        static constexpr uintmax_t         start_mask_  { detail::make_mask(start_bit) };
        static constexpr uintmax_t         last_mask_   { detail::make_mask(last_bit) };
        static constexpr detail::Direction direction_   { detail::direction(start_bit, last_bit) };
        integral_t                         &sink_;
        uintmax_t                          current_mask_{ start_mask_ };
    };
    
    template <std::integral integral_t, auto start_bit = 6, auto last_bit = 0>
    auto begin(integral_t &value) { return Bitout_integral_value<start_bit, last_bit, integral_t>{ value }; }

    
    /*
        Bitsource_forward_iter is an forward iterator that reads bits from a source container of integral values (an Integral_container).
        The caller specifies a range of bits of each source value.
    */
    
    template <auto start_bit = 6, auto last_bit = 0, Integral_container Container = std::string>
        requires std::is_integral_v<decltype(start_bit)> and std::is_integral_v<decltype(last_bit)>
    struct Bitsource_forward_iter
    {
        // Boiler plate iterator nested types.
        using iterator_category = std::forward_iterator_tag;
        using value_type        = bool;
        using difference_type   = std::ptrdiff_t;
        using reference         = bool const &;
        //using pointer           = bool const *;   // probably not needed

        Bitsource_forward_iter(): container_{ nullptr }                                      {}
        Bitsource_forward_iter( Container& container ):
            container_{ &container },
            cont_iter_{ begin(*container_) }
        {}
        Bitsource_forward_iter( Container& container, Container::const_iterator it):
            container_{ &container },
            cont_iter_{ it}
        {}

        //pointer operator->() { return &operator*(); }   // probably not needed
        reference operator*() const
        {
            assert(container_ and "Dereferencing iterator with no container.");
            assert((end(*container_) != cont_iter_) and "Dereferencing past end of container");
            current_value_ = *cont_iter_ & current_mask_;
            return current_value_;
        }
        Bitsource_forward_iter & operator++() /* prefix */
        {
            assert(container_ and "Incrementing iterator with no container.");
            assert((end(*container_) != cont_iter_) and "Incrementing past end of container");
            if (current_mask_ == last_mask_)
            {
                current_mask_ = start_mask_;
                ++cont_iter_;
            }
            else
            {
                increment_mask(current_mask_, direction_);
            }
            return *this;
        }
        Bitsource_forward_iter & operator++(int) /* postfix */
        {
            auto result{*this};
            ++this;
            return result;
        }
        bool operator==(Bitsource_forward_iter const &other) const
        {
            assert(((container_ == other.container_) or (not container_) or (not other.container_))
                    and ("Comparing iterators to different containers."));
            if (not other.container_)
            {
                return (not container_) or (end(*container_) == cont_iter_);
            }
            else
            {
                if (not container_)
                {
                    return (not other.container_) or (end(*other.container_) == other.cont_iter_);
                }
            }
            return container_ == other.container_ and cont_iter_ == other.cont_iter_ and current_mask_ == other.current_mask_;
        }
        bool operator!=(Bitsource_forward_iter const &other) const { return !(*this == other); }

    private:
        static constexpr uintmax_t         start_mask_  { detail::make_mask(start_bit) };
        static constexpr uintmax_t         last_mask_   { detail::make_mask(last_bit) };
        static constexpr detail::Direction direction_   { detail::direction(start_bit, last_bit) };
        Container                         *container_;
        typename Container::const_iterator cont_iter_;
        uintmax_t                          current_mask_{ start_mask_ };
        mutable bool                       current_value_{false};
    };
    
    template <auto start_bit = 0, auto last_bit = 7, Integral_container Container = std::string>
    auto begin(Container &c) { return Bitsource_forward_iter<start_bit, last_bit, Container>{ c }; }
    
    template <auto start_bit = 0, auto last_bit = 7, Integral_container Container = std::string>
    auto end(Container &)    { return Bitsource_forward_iter<start_bit, last_bit, Container>{}; }

} // namespace biterator namespace

