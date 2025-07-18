#include "testthat-helpers.h"
#include "covariance.h"

context("unstructured") {
  test_that("get_unstructured produces expected result") {
    vector<double> theta {{log(1.0), log(2.0), 3.0}};
    matrix<double> result = get_unstructured(theta, 2);
    matrix<double> expected(2, 2);
    expected <<
      1.0, 0.0,
      6.0, 2.0;
    expect_equal_matrix(result, expected);
  }
}

context("ante_dependence") {
  test_that("corr_fun_ante_dependence works as expected") {
    vector<double> theta {{1.0, 2.0}};
    corr_fun_ante_dependence<double> test_fun(theta);
    expect_equal(test_fun(1, 0), 0.7071068);
    expect_equal(test_fun(2, 0), 0.6324555);
    expect_equal(test_fun(2, 1), 0.8944272);
  }

  test_that("get_ante_dependence produces expected result") {
    vector<double> theta {{log(2.0), 1.0, 2.0}};
    matrix<double> result = get_ante_dependence(theta, 3);
    matrix<double> expected(3, 3);
    expected <<
      2.0, 0.0, 0.0,
      sqrt(2.0), sqrt(2.0), 0.0,
      1.264911, 1.264911, 0.8944272;
    expect_equal_matrix(result, expected);
  }

  test_that("get_ante_dependence_heterogeneous produces expected result") {
    vector<double> theta {{log(1.0), log(2.0), log(3.0), 1.0, 2.0}};
    matrix<double> result = get_ante_dependence_heterogeneous(theta, 3);
    matrix<double> expected(3, 3);
    expected <<
      1.0, 0.0, 0.0,
      sqrt(2.0), sqrt(2.0), 0.0,
      1.897367, 1.897367, 1.341641;
    expect_equal_matrix(result, expected);
  }
}

context("toeplitz") {
  test_that("corr_fun_toeplitz works as expected") {
    vector<double> theta {{1.0, 2.0}};
    corr_fun_toeplitz<double> test_fun(theta);
    expect_equal(test_fun(1, 0), 0.7071068);
    expect_equal(test_fun(2, 0), 0.8944272);
    expect_equal(test_fun(2, 1), 0.7071068);
  }

  test_that("get_toeplitz produces expected result") {
    vector<double> theta {{log(2.0), 1.0, 2.0}};
    matrix<double> result = get_toeplitz(theta, 3);
    matrix<double> expected(3, 3);
    expected <<
      2.0, 0.0, 0.0,
      sqrt(2.0), sqrt(2.0), 0.0,
      1.788854, 0.2111456, 0.8691476;
    expect_equal_matrix(result, expected);
  }

  test_that("get_toeplitz_heterogeneous produces expected result") {
    vector<double> theta {{log(1.0), log(2.0), log(3.0), 1.0, 2.0}};
    matrix<double> result = get_toeplitz_heterogeneous(theta, 3);
    matrix<double> expected(3, 3);
    expected <<
      1.0, 0.0, 0.0,
      sqrt(2.0), sqrt(2.0), 0.0,
      2.683282, 0.3167184, 1.303721;
    expect_equal_matrix(result, expected);
  }
}

context("autoregressive") {
  test_that("corr_fun_autoregressive works as expected") {
    vector<double> theta {{1.0}};
    corr_fun_autoregressive<double> test_fun(theta);
    expect_equal(test_fun(1, 0), 1 / sqrt(2));
    expect_equal(test_fun(4, 1), 0.3535534);
  }

  test_that("get_auto_regressive produces expected result") {
    vector<double> theta {{log(2.0), 3.0}};
    matrix<double> result = get_auto_regressive(theta, 3);
    matrix<double> expected(3, 3);
    expected <<
      2, 0, 0,
      1.89736659610103, 0.632455532033676, 0,
      1.8, 0.6, 0.632455532033676;
    expect_equal_matrix(result, expected);
  }

  test_that("get_auto_regressive_heterogeneous produces expected result") {
    vector<double> theta {{log(1.0), log(2.0), log(3.0), 2.0}};
    matrix<double> result = get_auto_regressive_heterogeneous(theta, 3);
    matrix<double> expected(3, 3);
    expected <<
      1, 0, 0,
      1.78885438199983, 0.894427190999916, 0,
      2.4, 1.2, 1.34164078649987;
    expect_equal_matrix(result, expected);
  }
}

context("compound symmetry") {
  test_that("corr_fun_compound_symmetry works as expected") {
    vector<double> theta {{1.2}};
    corr_fun_compound_symmetry<double> test_fun(theta, 3);
    expect_equal(test_fun(1, 0), 0.6527872);
    expect_equal(test_fun(4, 1), 0.6527872);
    expect_equal(test_fun(3, 1), 0.6527872);
  }

  test_that("corr_fun_compound_symmetry respects the lower boundary") {
    vector<double> theta {{-100.0}}; // So low that we get lower bound.
    corr_fun_compound_symmetry<double> test_fun(theta, 100);
    double lower_bound = - 1.0 / (100.0 - 1.0);
    expect_equal(test_fun(1, 0), lower_bound);
    expect_equal(test_fun(4, 1), lower_bound);
    expect_equal(test_fun(3, 1), lower_bound);
  }

  test_that("get_compound_symmetry produces expected result") {
    vector<double> theta {{log(2.0), 3.0}};
    matrix<double> result = get_compound_symmetry(theta, 3);
    matrix<double> expected(3, 3);
    expected <<
      2, 0, 0,
      1.8577223804673, 0.740855962458903, 0,
      1.8577223804673, 0.356766134631966, 0.649296143751581;
    expect_equal_matrix(result, expected);
  }

  test_that("get_compound_symmetry_heterogeneous produces expected result") {
    vector<double> theta {{log(1.0), log(2.0), log(3.0), - 2.0}};
    matrix<double> result = get_compound_symmetry_heterogeneous(theta, 3);
    matrix<double> expected(3, 3);
    expected <<
      1, 0, 0,
      -0.642391233933647, 1.89402573967864, 0,
      -0.963586850900471, -1.3443182923085, 2.50286010590613;
    expect_equal_matrix(result, expected);
  }
}

context("get_covariance_lower_chol") {
  test_that("get_covariance_lower_chol gives expected unstructured result") {
    vector<double> theta {{log(1.0), log(2.0), 3.0}};
    matrix<double> result = get_covariance_lower_chol(theta, 2, "us");
    matrix<double> expected = get_unstructured(theta, 2);
    expect_equal_matrix(result, expected);
  }
  // No other tests needed here for now.
}
