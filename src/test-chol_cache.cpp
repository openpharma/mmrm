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
    matrix<double> chols1 = chol.get_chol(vis, dist);
    expect_equal_matrix<matrix<double>>(chols1, expected);
    expect_equal_matrix<matrix<double>>(chol.chols[vis], expected);

    matrix<double> expected2(1, 1);
    expected2 << 1.0;
    std::vector<int> vis2{0};
    expect_equal_matrix<matrix<double>>(chol.get_chol(vis2, dist), expected2);
    expect_equal_matrix<matrix<double>>(chol.chols[vis2], expected2);
  }
}

context("get_chol_and_clean") {
  test_that("get_chol_and_clean works and cleans up all objects") {
    std::map<int, lower_chol_base<double>*> chols_by_group;
    vector<double> theta {{log(1.0), log(2.0), 3.0, log(1.0), 0.0}};
    chols_by_group[0] = new lower_chol_nonspatial<double>(theta.segment(0, 3), 2, "us");
    matrix<double> ret1 = get_chol_and_clean(chols_by_group, false, 2);
    matrix<double> expected1(2, 2);
    expected1 <<
      1.0, 0.0,
      6.0, 2.0;
    expect_equal_matrix<matrix<double>>(ret1, expected1);
    expect_equal(0, int(chols_by_group.size()));
    
    chols_by_group[0] = new lower_chol_spatial<double>(theta.segment(3, 2), "sp_exp");
    matrix<double> ret2 = get_chol_and_clean(chols_by_group, true, 2);
    matrix<double> expected2(2, 2);
    expected2 <<
      1.0, 0,
      0.5, sqrt(3.0) / 2.0;
    expect_equal_matrix<matrix<double>>(ret2, expected2);
    expect_equal(0, int(chols_by_group.size()));
  }
}