// Copyright 2025 (c) Grant Rostig, grantrostig.com, Boost 1.0 license
#ifndef RANDOM_TOOLKIT_H
#define RANDOM_TOOLKIT_H
#include "../grostig_tools/cpp_headers.hpp"

/*
Copyright [yyyy] [name of copyright owner]

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

 * A function toolkit
 *
 * // inspired by W.E. Brown and his WG21 N3847 from 2014

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

=========================================
Grant's communication with author and his granting of permission to use the
software.
=========================================

Delivered-To: grant@grantrostig.com
Received: by 10.176.85.211 with SMTP id w19csp870020uaa;
        Sat, 9 Dec 2017 08:38:58 -0800 (PST)
X-Received: by 10.107.47.93 with SMTP id j90mr45702754ioo.262.1512837538265;
        Sat, 09 Dec 2017 08:38:58 -0800 (PST)
ARC-Seal: i=1; a=rsa-sha256; t=1512837538; cv=none;
        d=google.com; s=arc-20160816;
        b=ElxN5BSycecpG1+/GOb1bBzIaaIzxcrB3/i8tjETBItHo2wHkwRK9rpmA4NmvxakCL
         24HbiHault1FgXOFcyZ/MLn5deeoL2yxM+W9scZub5I7u3qklAKTc2k7HH1NNVuJK1o0
         zULNaMbUI7kCs5LK3toUHftHA3Bm/Q7Fux+qODmTdbf3veHg4RgFkAi02ZzRdUDBFyLw
         K9rPIPj+QcbWd/TqsSLb3lC7N4KLYUiPLDZWlByQBPQp+ZTT+Ue/NXXFcaY4sZtWHLwK
         xnxATbuG0ik913eXHcli4HXD1GKlZi04XvMVNSauZ/JRp1xAqyQzgH6aTl1ySBHZ2H+H
         Ijyw==
ARC-Message-Signature: i=1; a=rsa-sha256; c=relaxed/relaxed; d=google.com; s=arc-20160816;
        h=to:references:message-id:content-transfer-encoding:date:in-reply-to
         :from:subject:mime-version:dkim-signature:arc-authentication-results;
        bh=wwj8g8v/xqOZXsLi8TDy2/rfXEdHSBbb6eP7asLMEAo=;
        b=MFPLgqZQ+NH2KUq//QhrSQlQNYao9tJRzkDw6AVNjxru8nrQUSSKq47CZgMLHxpUjQ
         mLidyx8acVabf8N2sbSX2NBQypeIj/al/RWZnG3QmYAkHxW8Tl1Bb+PP/le2eXko9XAy
         jBIT0NQCnoxWlKQprMN6sAvUkENi0UzN6H66q5I+vJu+1oEEB4kFg6K19sAM/PbYxtJH
         hNTnkvp7dxAoiEfFu6fmqGSVYtJZXDpHWY44b9o93g/T4Ycb06Z+/oc3IcnTwb+1tPs6
         gYM9D+Bczp59RM8h3ZC/9TarPS7x9xE68aE8/VEbRBbzWuGm41iyP/ZYDh+4HYcwP4SY
         FV/w==
ARC-Authentication-Results: i=1; mx.google.com;
       dkim=pass header.i=@gmail.com header.s=20161025 header.b=QraVo2eE;
       spf=pass (google.com: domain of webrown.cpp@gmail.com designates 209.85.220.41 as permitted sender) smtp.mailfrom=webrown.cpp@gmail.com;
       dmarc=pass (p=NONE sp=NONE dis=NONE) header.from=gmail.com
Return-Path: <webrown.cpp@gmail.com>
Received: from mail-sor-f41.google.com (mail-sor-f41.google.com. [209.85.220.41])
        by mx.google.com with SMTPS id d16sor1476387itc.28.2017.12.09.08.38.57
        for <Grant@grantrostig.com>
        (Google Transport Security);
        Sat, 09 Dec 2017 08:38:58 -0800 (PST)
Received-SPF: pass (google.com: domain of webrown.cpp@gmail.com designates 209.85.220.41 as permitted sender) client-ip=209.85.220.41;
Authentication-Results: mx.google.com;
       dkim=pass header.i=@gmail.com header.s=20161025 header.b=QraVo2eE;
       spf=pass (google.com: domain of webrown.cpp@gmail.com designates 209.85.220.41 as permitted sender) smtp.mailfrom=webrown.cpp@gmail.com;
       dmarc=pass (p=NONE sp=NONE dis=NONE) header.from=gmail.com
DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;
        d=gmail.com; s=20161025;
        h=mime-version:subject:from:in-reply-to:date
         :content-transfer-encoding:message-id:references:to;
        bh=wwj8g8v/xqOZXsLi8TDy2/rfXEdHSBbb6eP7asLMEAo=;
        b=QraVo2eEm+0KAR54XSDSNOESA2PHFr4nAUGLh3TiHEdKvD5BVwDiK/Tc1Gc0Yo7TRL
         kE9hM1hW4xNeRO1VsPHGUToorlEXMJS5dTgTED3AAG1k1qFhSDtNrsv+XUTis7p6o8Do
         3B4je4jkwoNMVebD3yD/MkXALrWXjLELG3RcoV4KlbLB8MZM14OooB4YKage4Dam2RST
         zm8aw1mP/KTX39lKNA37Rya6MSN8MWmnN/W0FgICpj1HtVnpklpuhYwTGoAqHauMAa6j
         MoVXhvMHBjkxmZdLV5NyBgSKFdkHS1AdC9GFVpyv7yIi4rUYuO/rSXRGhb74Uy1PjU+r
         JGPA==
X-Google-DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;
        d=1e100.net; s=20161025;
        h=x-gm-message-state:mime-version:subject:from:in-reply-to:date
         :content-transfer-encoding:message-id:references:to;
        bh=wwj8g8v/xqOZXsLi8TDy2/rfXEdHSBbb6eP7asLMEAo=;
        b=giyGjNQkOsnGFV1LepSsHd0bJUsSxe3OtZnf7eDnyQffYHLTuX62gG1CCGkrJnsBSl
         08CqEFvtnu0+2tL3EGZ4Y7ZseRFPh1rG4aMJfhOfKSeSIvSDvQeaK5pupnYI7Ko7+NwB
         /w2qv9WHvv8tTVZ1Mf+vRaviLwgd7zY6mHyG15a1epf+w/1IFK3P/gC6rCEGi2mk1BDh
         maMpHD9UAg+BphwCkerxQp/MIBC1UAdva0YgTujt2sIfb+bkwELNOXevyMC3QUS6fyIn
         8dBRibcVM5lMB7A7DSEY4n8BNybVHzkDXKvpHzSAJxmwXW/Uq/Lfg7+0DVMVJRH7YGkZ
         BVJA==
X-Gm-Message-State: AKGB3mLsnsOibR9LVgEzo6TJdHZHlITUxVUAeoWnJAAUYEZ4M77EFc/Z rnftyjj14E3AzodpBItQLXTr1g==
X-Google-Smtp-Source: AGs4zMZPdI7E3FyBvlUYpls0Oj12OvnZFAlZi3/TeOZR7/dGxpD4NY9olRzrgB47fxMGLtR/use/2g==
X-Received: by 10.36.135.199 with SMTP id f190mr9983707ite.133.1512837537731;
        Sat, 09 Dec 2017 08:38:57 -0800 (PST)
Return-Path: <webrown.cpp@gmail.com>
Received: from ?IPv6:2601:249:602:566e:241c:d7a:b650:eb51? ([2601:249:602:566e:241c:d7a:b650:eb51])
        by smtp.gmail.com with ESMTPSA id h19sm1828648iod.85.2017.12.09.08.38.56
        (version=TLS1 cipher=ECDHE-RSA-AES128-SHA bits=128/128);
        Sat, 09 Dec 2017 08:38:57 -0800 (PST)
Content-Type: text/plain; charset=us-ascii
Mime-Version: 1.0 (Mac OS X Mail 8.0 \(1990.1\))
Subject: Re: N3847 and N3551 license for the code?
From: W Brown <webrown.cpp@gmail.com>
In-Reply-To: <1ed17833-5871-465f-476b-88fc16483f73@GrantRostig.com>
Date: Sat, 9 Dec 2017 10:38:56 -0600
Content-Transfer-Encoding: quoted-printable
Message-Id: <10901347-F0ED-4852-A8C0-6AD4839CE39A@gmail.com>
References: <1ed17833-5871-465f-476b-88fc16483f73@GrantRostig.com>
To: Grant@grantrostig.com
X-Mailer: Apple Mail (2.1990.1)

Hello, and thanks for writing.  I'm pleased that you found my papers & vide=
os of interest.

You are welcome to Fair Use of those materials subject to proper attributio=
n, with any changes you may make clearly identified as not my work, and wit=
h the understanding that you do so on an "as-is" basis without warranties o=
f any kind and that you assume any and all risk related to such Use.  In ca=
se of any dispute, the terms and conditions of the Apache License Version 2=
.0 at https://www.apache.org/licenses/LICENSE-2.0.html shall govern.

Submission to "the C++ committee" does not affect your situation.

Feel free to contact me again re this or any other C++-related matter.

With best wishes of the season,

-- WEB


> On Dec 8, 2017, at 3:54 PM, Grant Rostig <grant@grantrostig.com> wrote:
>=20
> Hi Walter,
>=20
> Thank you for writing N3847 and  N3551, as well as the videos on the inte=
rnet.
>=20
> I would like to use your code and some of the text of those documents (as=
 comments embedded into the code) in a commercial programming project.
>=20
> You placed a copyright on the documents and I'm not sure about the licens=
e for the code.
>=20
> Can you clarify?  Does it make a difference that you submitted it to the =
C++ committee?
>=20
> Thanks for your consideration.
>=20
> ++Regards,
>=20
> --=20
> Grant Rostig
> 512-394-3621
> Grant@GrantRostig.com
>=20

*/

namespace grostig {

std::mt19937_64 &global_urbg();  // forward

void randomize();

// The templated version, a non-templated version exits in .cpp
template <class T>
std::enable_if_t<std::is_integral<T>{}(), T>
    pick_a_number_t(T from, T thru) {  // why does qtcreator show error for this line?  todo
  static std::uniform_int_distribution<T> d{};
  using parm_t = typename decltype(d)::param_type; // had to add typename to resolve compile error?  todo
  return d(global_urbg(), parm_t{from, thru});
}

template <class T>
std::enable_if_t<std::is_floating_point<T>{}(), T>
    pick_a_number_t(T from, T upto) {
  static std::uniform_real_distribution<T> d{};
  using parm_t = typename decltype(d)::param_type; // had to add typename to resolve compile error?  todo
  return d(global_urbg(), parm_t{from, upto});
}

/* ======================================
 * everything below to end of file is
 * commented out
 * ======================================
 *
 *  class toolkit

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

/*
class random_number_source {
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
/*
using
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

/*
Note that the use of reference semantics has again made the user responsible for
instantiating
and maintaining ownership of the underlying URBG and distribution. However, it
is typical for
iterators not to own their referents, so such responsibility should be no
surprise to users:
*/

/*
template< class Distribution = uniform_int_distribution<int>
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

} // namespace grostig

#endif // RANDOM_TOOLKIT_H
