#include "testthat-helpers.h"
#include "chol_cache.h"

context("cholesky cache") {
  test_that("cached cholesky stores result correctly") {
    vector<double> theta {{log(1.0), log(2.0), 3.0}};
    auto chol = lower_chol_nonspatial<double>(theta, 2, "us");
    matrix<double> expected(2, 2);
    expected <<
      1.0, 0.0,
      6.0, 2.0;
    std::vector<int> vis{0, 1};
    matrix<double> dist;
    expect_equal_matrix(chol.get_chol(vis, dist), expected);
    expect_equal_matrix(chol.chols[vis], expected);

    matrix<double> expected2(1, 1);
    expected2 << 1.0;
    std::vector<int> vis2{0};
    expect_equal_matrix(chol.get_chol(vis2, dist), expected2);
    expect_equal_matrix(chol.chols[vis2], expected2);
  }
}
