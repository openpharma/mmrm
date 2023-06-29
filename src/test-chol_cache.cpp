#include "testthat-helpers.h"
#include "chol_cache.h"

context("cholesky cache") {
  test_that("cached cholesky stores result correctly") {
    vector<double> theta {{log(1.0), log(2.0), 3.0}};
    auto chol = lower_chol_nonspatial<double>(theta, 2, "us");
    matrix<double> chol1_expected(2, 2);
    chol1_expected <<
      1.0, 0.0,
      6.0, 2.0;
    std::vector<int> vis{0, 1};
    matrix<double> dist;
    expect_equal_matrix(chol.get_chol(vis, dist), chol1_expected);
    expect_equal_matrix(chol.chols[vis], chol1_expected);

    matrix<double> simga1_expected(2, 2);
    simga1_expected <<
      1.0, 6.0,
      6.0, 40.0;
    expect_equal_matrix(chol.get_sigma(vis, dist), simga1_expected);
    expect_equal_matrix(chol.sigmas[vis], simga1_expected);

    matrix<double> simga1_inv = chol.get_sigma_inverse(vis, dist);
    matrix<double> simga1_inv_expected(2, 2);
    simga1_inv_expected <<
      10.0, -1.5,
      -1.5, 0.25;
    expect_equal_matrix(simga1_inv, simga1_inv_expected);
    expect_equal_matrix(chol.sigmas_inv[vis], simga1_inv_expected);

    matrix<double> chol2_expect(1, 1);
    chol2_expect << 1.0;
    std::vector<int> vis2{0};
    expect_equal_matrix(chol.get_chol(vis2, dist), chol2_expect);
    expect_equal_matrix(chol.chols[vis2], chol2_expect);

    matrix<double> sigma2_expect(1, 1);
    sigma2_expect << 1.0;
    expect_equal_matrix(chol.get_sigma(vis2, dist), sigma2_expect);
    expect_equal_matrix(chol.sigmas[vis2], sigma2_expect);

    matrix<double> sigma2_inv_expect(1, 1);
    sigma2_inv_expect << 1.0;
    expect_equal_matrix(chol.get_sigma_inverse(vis2, dist), sigma2_inv_expect);
    expect_equal_matrix(chol.sigmas_inv[vis2], sigma2_inv_expect);
  }
}

context("cholesky group object") {
  test_that("cholesky group return result correctly") {
    vector<double> theta {{log(1.0), log(2.0), 3.0, log(2.0), log(4.0), 5}};
    auto chol_group = chol_cache_groups<double>(theta, 2, false, "us", 2);
    matrix<double> chol1_expected(2, 2);
    chol1_expected <<
      1.0, 0.0,
      6.0, 2.0;
    std::vector<int> vis{0, 1};
    matrix<double> dist;
    expect_equal_matrix(chol_group.cache[0]->get_chol(vis, dist), chol1_expected);
    matrix<double> chol2_expected(2, 2);
    chol2_expected <<
      2.0, 0.0,
      20.0, 4.0;
    expect_equal_matrix(chol_group.cache[1]->get_chol(vis, dist), chol2_expected);
  }
}
