#ifndef TESTTHAT_WRAP_H
#define TESTTHAT_WRAP_H
#include <testthat.h>
#include <limits>
#include "tmb_includes.h"


#define expect_equal(TARGET, CURRENT)                          \
{                                                              \
  double const eps =                                           \
    std::sqrt(std::numeric_limits<double>::epsilon());         \
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
void expect_equal_matrix(const matrix<T>& target, const matrix<T>& current)
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

#endif
