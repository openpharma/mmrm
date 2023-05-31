#ifndef TESTTHAT_WRAP_H
#define TESTTHAT_WRAP_H
#include <testthat.h>
#include <limits>
#include "utils.h"

// Expect equal: Here use a default epsilon which gives around 1e-4 on
// my computer here.
#define expect_equal(TARGET, CURRENT)                          \
{                                                              \
  double const eps =                                           \
    std::pow(std::numeric_limits<double>::epsilon(), 0.25);    \
                                                               \
  if(std::abs((TARGET)) > eps)                                 \
    expect_true(std::abs((TARGET) - (CURRENT)) /               \
      std::abs((TARGET)) < eps);                               \
  else                                                         \
    expect_true(std::abs((TARGET) - (CURRENT)) < eps);         \
}

#define expect_equal_eps(TARGET, CURRENT, EPS)                 \
{                                                              \
  if(std::abs((TARGET)) > (EPS))                               \
    expect_true(std::abs((TARGET) - (CURRENT)) /               \
      std::abs((TARGET)) < (EPS));                             \
  else                                                         \
    expect_true(std::abs((TARGET) - (CURRENT)) < (EPS));       \
}

template <class T>
void expect_equal_matrix(const T& target, const T& current)
{
  int nrow = target.rows();
  int ncol = target.cols();

  expect_true(nrow == current.rows());
  expect_true(ncol == current.cols());

  for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < ncol; j++) {
      expect_equal(target(i, j), current(i, j));
    }
  }
}

template <class T>
void expect_equal_vector(const T& target, const T& current)
{
  int n = target.size();
  expect_true(n == current.size());

  for (int i = 0; i < n; i++) {
    expect_equal(target(i), current(i));
  }
}

#endif
