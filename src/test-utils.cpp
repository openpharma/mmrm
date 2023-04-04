#include "testthat-helpers.h"
#include "utils.h"

context("get_select_matrix") {
  test_that("get_select_matrix works as expected") {
    vector<int> visits_i1 {{0, 3, 5}};
    Eigen::SparseMatrix<double> result = get_select_matrix<double>(visits_i1, 7);
    matrix<double> expected(3, 7);
    expected <<
      1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0;
    matrix<double> result_dense(result);
    expect_equal_matrix(result_dense, expected);
    std::vector<int> visits_i2 {{0, 3, 5}};
    Eigen::SparseMatrix<double> result2 = get_select_matrix<double>(visits_i2, 7);
    matrix<double> result_dense2(result2);
    expect_equal_matrix(result_dense2, expected);
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

context("map_to_cor") {
  test_that("map_to_cor works as expected") {
    vector<double> theta {{-5., 2., 10., 0.}};
    vector<double> result = map_to_cor(theta);
    // Expected from R:
    // test <- c(-5, 2, 10, 0)
    // test / sqrt(1 + test^2)
    vector<double> expected {{-0.98058067569092, 0.894427190999916, 0.995037190209989, 0.0}};
    expect_equal_vector(result, expected);
  }
}

context("generic_corr_fun") {
  test_that("generic_corr_fun is initialized as expected") {
    vector<double> theta {{-5., 2., 10., 0.}};
    generic_corr_fun<double> result(theta);
    vector<double> expected_corr_values = map_to_cor(theta);
    expect_equal_vector(result.corr_values, expected_corr_values);
  }
}

template <class T>
struct const_cor {
  const T operator() (int& i, int& j) const {
    return T(0.5);
  }
};
context("get_corr_mat_chol") {
  test_that("get_corr_mat_chol works as expected") {
    const_cor<double> const_fun;
    matrix<double> result = get_corr_mat_chol(3, const_fun);
    matrix<double> expected(3, 3);
    expected <<
      1, 0, 0,
      0.5, 0.866025403784439, 0,
      0.5, 0.288675134594813, 0.816496580927726;
    expect_equal_matrix(result, expected);
  }
}

template <class T>
struct test_cor {
  const T operator() (int& i, int& j) const {
    return T(0.0);
  }
};
context("get_heterogeneous_cov") {
  test_that("get_heterogeneous_cov works as expected") {
    vector<double> sd_values {{1., 2., 3.}};
    test_cor<double> test_fun;
    matrix<double> result = get_heterogeneous_cov(sd_values, test_fun);
    matrix<double> expected(3, 3);
    expected <<
      1.0, 0.0, 0.0,
      0.0, 2.0, 0.0,
      0.0, 0.0, 3.0;
    expect_equal_matrix(result, expected);
  }
}

context("euclidean distance") {
  test_that("euclidean works as expected") {
    matrix<double> coord(4, 1);
    coord << 1, 2, 3, 4;
    matrix<double> expected(4, 4);
    expected <<
      0, 1, 2, 3,
      1, 0, 1, 2,
      2, 1, 0, 1,
      3, 2, 1, 0;
    expect_equal_matrix(euclidean(coord), expected);
  }
  test_that("euclidean works as expected for matrix") {
    matrix<double> coord(4, 2);
    coord << 1, 2, 3, 4, 5, 6, 7, 8;
    matrix<double> expected(4, 4);
    expected <<
      0, 2, 4, 6,
      2, 0, 2, 4,
      4, 2, 0, 2,
      6, 4, 2, 0;
    expected = expected * sqrt(2);
    expect_equal_matrix(euclidean(coord), expected);
  }
}
