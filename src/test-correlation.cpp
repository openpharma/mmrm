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


context("tcrossprod") {
  test_that("tcrossprod works as expected with complete") {
    matrix<double> lower_chol(2, 2);
    lower_chol <<
      1.0, 0.0,
      6.0, 2.0;
    matrix<double> result = tcrossprod(lower_chol, true);
    matrix<double> expected = lower_chol * lower_chol.transpose();
    expect_equal_matrix(result, expected);
  }

  test_that("tcrossprod works as expected without complete (default)") {
    matrix<double> lower_chol(2, 2);
    lower_chol <<
      1.0, 0.0,
      6.0, 2.0;
    matrix<double> result = tcrossprod(lower_chol); // default: no complete.
    matrix<double> full = lower_chol * lower_chol.transpose();
    matrix<double> expected = full.template triangularView<Eigen::Lower>();
    expect_equal_matrix(result, expected);
  }
}

context("crossprod") {
  test_that("crossprod works as expected") {
    matrix<double> x(2, 3);
    x <<
      1.0, 0.0, 1.0,
      6.0, 2.0, 4.2;
    matrix<double> result = crossprod(x);
    matrix<double> full = x.transpose() * x;
    matrix<double> expected = full.template triangularView<Eigen::Lower>();
    expect_equal_matrix(result, expected);
  }
}
