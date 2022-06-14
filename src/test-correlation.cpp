#include "testthat-helpers.hpp"
#include "correlation.hpp"

context("get_unstructured") {
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

// Add unit tests for other covariance functions here.

context("get_covariance_lower_chol") {
  test_that("get_covariance_lower_chol gives expected unstructured result") {
    vector<double> theta {{log(1.0), log(2.0), 3.0}};
    matrix<double> result = get_covariance_lower_chol(theta, 2, unstructured_corr);
    matrix<double> expected = get_unstructured(theta, 2);
    expect_equal_matrix(result, expected);
  }

  // Add unit tests for other cases here.
}

context("get_select_matrix") {
  test_that("get_select_matrix works as expected") {
    vector<int> visits_i {{0, 3, 5}};
    Eigen::SparseMatrix<double> result = get_select_matrix<double>(visits_i, 7);
    matrix<double> expected(3, 7);
    expected <<
      1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0;
    matrix<double> result_dense(result);
    expect_equal_matrix(result_dense, expected);
  }
}
