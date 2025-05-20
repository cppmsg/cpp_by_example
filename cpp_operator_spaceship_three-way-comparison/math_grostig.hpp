/* math_grostig.h - Provides some math functions not yet found in production libraries

   Copyright (c) 2017 Grant Rostig. All rights reserved.
   License: Boost v1.0
   Permission is granted to use these files during and in support of CppMSG meetup group events and projects.

   Inspired by: Learning from Data AI book and online MOOC? course.
   Shows how to:
*/
#pragma once
#include "cpp_headers.hpp"
static const double grostig_pi = 3.141592653589793;

double n_Choose_k(unsigned long const n, unsigned long k);

double n_Choose_kf(double const n, double k);

double factorial(unsigned long const x, unsigned long const result = 1);

double m_subH_max(unsigned long const n_uc, unsigned long const vc_dim);

double m_subH_maxf(double const n_uc, double const vc_dim);

double m_subH_max_est(double const n_uc, double const vc_dim);

double ge_converge1(double vc_dim, unsigned long n, double delta);

double ge_converge2(double vc_dim, double n, double delta);
