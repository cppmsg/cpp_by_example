// Copyright 2025 (c) Grant Rostig, grantrostig.com, Boost 1.0 license
/* Copyright [yyyy] [name of copyright owner]

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

See header file for more detail.

============================================

A function toolkit

  // inspired by W.E. Brown and his WG21 N3847 from 2014

    In [N3547] and its successor [N3742], we proposed “to add to <random> the
following modesttoolkit of novice-friendly functions:
• global_urng() “Grants access to a URNG object of implementation-specified
type.
• randomize() “Sets the above URNG object to an unpredictable state. 11
• pick_a_number(from,thru) “Returns an int variate uniformly distributed in the
closed int range [from,
thru].
• pick_a_number(from,upto) “Returns a double variate uniformly distributed in
the half-open double range
[from, upto).”
We had provided, in that proposal, the following sample implementation (here
slightly edited for consistency with our revised URBG nomenclature as described
above).

    Such a toolkit follows the basic recommendation, but does so in a fashion
that provides several advantages over the single-function approach illustrated
earlier. For example, the URBG can at any time be reseeded (reinitialized):
• to a known state via global_urbg().seed(· · ·) in order to ensure
reproducibility, or
• to an unknown state via randomize() in order to avoid reproducibility.
“Unpredictability is the ideal. Using a computer, we typically settle for
very-very-very-very-hard-to-predict.”N3847: Random Number Generation is Not
Simple! Further, a distribution is not limited to a single range of values, as
the desired range is provided per call via pick_a_number’s function arguments.
However, this design means the function is poorly suited for use with most standard library algorithms, as they
tend to expect niladic function objects as arguments.
*/

#include "random_toolkit.hpp"
namespace grostig {

std::mt19937_64 & global_urbg() {
// static std::default_random_engine u{};
  static std::mt19937_64 u {};
  return u;
}

void randomize() {
  static std::random_device rd {};
  global_urbg().seed( rd() );
}

/*  The overloaded pick_a_number functions below are reformulated as
    function templates in the header. This approach allows callers the additional freedom to
    specify their desired return type.

int pick_a_number(int from, int thru) {
  static std::uniform_int_distribution<> d{};
  using parm_t = decltype(d)::param_type;  // why does qtcreator show error for this line?  todo
  return d(global_urbg(), parm_t{from, thru});
}

double pick_a_number(double from, double upto) {
  static std::uniform_real_distribution<> d{};
  using parm_t = decltype(d)::param_type;
  return d(global_urbg(), parm_t{from, upto});
} */

/* class toolkit

    The following approach features a class interface to random variate
   generation. Despite its simplicity of use for simple tasks, it offers
   considerable flexibility for
   configuring its internal URBG and distribution resources.

        Although an object of such a type is a niladic function object, it is
   still somewhat poorly suited for use with most standard library algorithms in
   all but the very
   simplest of use cases. The issue        is that such stateful objects should
   nearly always be passed to an
   algorithm by reference, but        of course this is not the default
   parameter-passage mechanism used by
   the standard library for        function objects. It is therefore up to the
   user to wrap such an object
   g (for example, as ref(g))N3847: Random Number Generation is Not Simple!
   at each point of call, lest the URBG and distribution members be copied
   and so make possible        unexpected duplicate sequences of variates.

*/

/* class random_number_source {
public:
  // types
  using urbg_type = std::default_random_engine;
  using distribution_type = std::uniform_int_distribution<int>;
  using seed_type = typename urbg_type::result_type;
  using param_type = typename distribution_type::param_type;
  using result_type = typename distribution_type::result_type;

private:
  urbg_type e;
  distribution_type d;

public:
  // construct
  random_number_source() = default;
  random_number_source(seed_type seed) : e{seed}, d{} {}
  // use compiler-generated copy/move/destroy
  // reinitialize
  random_number_source &seed(seed_type seed) {
    e.seed(seed);
    d.reset();
    return *this;
  }
  random_number_source &randomize() { return seed(std::random_device{}()); }
  template <class P0, class... P1toN>
  param_type param(P0 &&p0, P1toN &&... p1ton) {
    param_type p = d.param();
    d.param(param_type(std::forward<P0>(p0), std::forward<P1toN...>(p1ton...)));
    d.reset();
    return p;
  }

  // produce random variate
  result_type operator()() { return d(e); }
  template <class P0, class... P1toN>
  result_type operator()(P0 p0, P1toN... p1ton) {
    return d( e, param_type(std::forward<P0>(p0), std::forward<P1toN...>(p1ton...)) );
  }

  // observe
  urbg_type &urbg() { return e; }
  distribution_type &distribution() { return d; }

  // equality-compare
  bool operator==(random_number_source const &other) {
    return e == other.e and d == other.d;
  }
  bool operator!=(random_number_source const &other) {
    return not(*this == other);
  }
};
*/

/* A class template toolkit
We note in passing that the class presented in the previous section can be
reformulated as a class
template. Such a variation provides users the additional capability of
specifying their desired
URBG and distribution types, yet providing as defaults those most commonly
wanted. We show the
template’s initial part only, as the bulk of the code duplicates that of the
class exhibited above:

template< class URBG
= default_random_engine
, class Distribution = uniform_int_distribution<int>
>
class random_number_source
{
private:
URBG
e;
Distribution d;
13 public:
// types
using urbg_type
= URBG;
using distribution_type = Distribution;
// etc.

While this reformulation does provide some additional flexibility, users must
still exercise care
whenever passing an object of such a type, for the same reasons stated above.
An iterator toolkit
We now present yet another approach, featuring
• an iterator interface to the generation of random variates, and
• internal reference semantics to preempt issues caused by copying.
An iterator interface can be advantageous in connection with such algorithms as
copy_n; here
are several examples of such use:
*/
/* using
using
using
using
rdev_t
urbt_t
dist_t
variate_t
=
=
=
=
random_device;
default_random_engine;
uniform_real_distribution<double>;
dist_t::result_type;
urbt_t u{ rdev_t{}() };
dist_t d{ };
variate_iterator<urbt_t,dist_t> it{u, d};
// see below
// create a random 5 dimensional vector:
vector<variate_t> v( 5 );
copy_n(it, 5, v.begin());
12 See also the std-discussion newsgroup thread starting with “How to avoid
accidental copying of random number
generators” (Christopher Jefferson, 2013-12-03).8

// dot product with a random vector:
variate_t prod{ inner_product( v.begin(), v.end(), it, 0.0) };
// add noise to a vector:
transform( v.begin(), v.end()
, v.begin()
, [&] ( variate_t d ) { return d + *it++; }
);
*/

/* Note that the use of reference semantics has again made the user responsible for
instantiating
and maintaining ownership of the underlying URBG and distribution. However, it
is typical for
iterators not to own their referents, so such responsibility should be no
surprise to users:
*/

/*  template< class Distribution = uniform_int_distribution<int>
, class URBG
= default_random_engine
>
class variate_iterator
: public iterator< input_iterator_tag, typename D::result_type >
{
private:
using iter_t = variate_iterator;
using val_t = typename D::result_type;
using ptr_t = val_t const*;
using ref_t = val_t const&;
URBG
*
Distribution *
val_t
bool
u{ nullptr }; // non-owning
d{ nullptr }; // non-owning
v{ };
valid{ false };
// help:
void
step( ) noexcept
ref_t deref( )

{ valid = false; }
{ if( not valid )
v = (*d)(*u), valid = true;
return v;
}
o ) const noexcept
d == o.d)
and *u == *o.u
and *d == *o.d
bool
eq( iter_t const&
{ return ( (u == o.u and
or (
u and o.u
and d and o.d
) )
and valid ? (v == o.v) : true
and valid == o.valid;
}
public:
// construct:
constexpr variate_iterator( ) noexcept = default;
variate_iterator( URBG& u, Distribution& d ) : u{ &u }, d{ &d }
// use compiler-generated copy/move/destroy
// dereference:
val_t operator * ( )
ptr_t operator -> ( )

{ return deref(); }
{ return &deref(); }
{ }N3847: Random Number Generation is Not Simple!
// advance:
iter_t& operator ++ (
)
iter_t
operator ++ ( int )
{ step(); return *this; }
{ iter_t tmp{*this}; step(); return tmp; }
// equality-compare:
bool operator == ( iter_t const& other ) const
bool operator != ( iter_t const& other ) const

{ return
eq(other); }
{ return not eq(other); }
};
*/

}

