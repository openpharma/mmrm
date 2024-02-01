#include "testthat-helpers.h"
#include "utils.h"

using namespace Rcpp;

context("subset_matrix") {
  test_that("subset_matrix works as expected") {
    matrix<double> mat(3, 3);
    mat <<
      1.0, 0.0, 0.5,
      6.0, 2.0, 1.0,
      3.0, 0.1, 0.2;
    std::vector<int> index {1, 0};
    matrix<double> result1 = subset_matrix(mat, index, index);
    matrix<double> exp1(2, 2);
    exp1 <<
      2.0, 6.0,
      0.0, 1.0;
    expect_equal_matrix(result1, exp1);

    matrix<double> result2 = subset_matrix(mat, index);

    matrix<double> exp2(2, 3);
    exp2 <<
      6.0, 2.0, 1.0,
      1.0, 0.0, 0.5;
    expect_equal_matrix(result2, exp2);
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

context("cpow works") {
  test_that("cpow gives correct power by element for power 0.5") {
    matrix<double> tmb_mat(4, 2);
    tmb_mat << 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0;
    matrix<double> expected(4, 2);
    expected << 1.0, sqrt(2.0), sqrt(3.0), 2.0, sqrt(5.0), sqrt(6.0), sqrt(7.0), sqrt(8.0);
    expect_equal_matrix(as_matrix<matrix<double>, Eigen::Matrix<double, -1, -1>>(cpow(as_matrix<Eigen::Matrix<double, -1, -1>, matrix<double>>(tmb_mat), 0.5)), expected);
  }
  test_that("cpow gives correct power by element for power 2") {
    matrix<double> tmb_mat(4, 2);
    tmb_mat << 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0;
    matrix<double> expected(4, 2);
    expected << 1.0, 4.0, 9.0, 16.0, 25.0, 36.0, 49.0, 64.0;
    expect_equal_matrix(as_matrix<matrix<double>, Eigen::Matrix<double, -1, -1>>(cpow(as_matrix<Eigen::Matrix<double, -1, -1>, matrix<double>>(tmb_mat), 2.0)), expected);
  }
}

context("pseudoInverseSqrt works") {
  test_that("pseudoInverseSqrt gives correct result") {
    matrix<double> tmb_mat(3, 3);
    tmb_mat << 5.483417,  2.861011,  3.478399,
      2.861011,  3.169936, -1.075550,
      3.478399, -1.075550, 10.525825;

    matrix<double> expected(3, 3);
    expected << 0.8235633, -0.5514385, -0.2586037,
      -0.5514385,  1.0568775,  0.2548210,
      -0.2586037,  0.2548210,  0.4095994;
    expect_equal_matrix(pseudoInverseSqrt(tmb_mat), expected);
  }

  test_that("pseudoInverseSqrt gives correct result for rank-deficient matrix") {
    matrix<double> tmb_mat(3, 3);
    tmb_mat << 5.483417,  2.861011,  0,
      2.861011,  3.169936, 0,
      0, 0, 0;

    matrix<double> expected(3, 3);
    expected << 0.5331152, -0.2459070,    0.0,
      -0.2459070, 0.7319613,    0.0,
      0.0000000,  0.0000000,    0.0;
    expect_equal_matrix(pseudoInverseSqrt(tmb_mat), expected);
  }
}

context("Rcpp and eigen conversion") {
  test_that("conversions do not change values") {
    NumericVector v1 = NumericVector::create(1.0, 2.0, 3.0);
    vector<double> v1_vec = as_vector< vector<double>, NumericVector>(v1);
    NumericVector v2 = as_vector<NumericVector, vector<double>>(v1_vec);
    vector<double> v3(3);
    v3 << 1.0, 2.0, 3.0;
    expect_equal_vector(v1_vec, v3);
    expect_equal_vector(v1, v2);

    IntegerVector v4 = IntegerVector::create(1, 2, 3);
    vector<int> v4_vec = as_vector<vector<int>, IntegerVector>(v4);
    IntegerVector v5 = as_vector<IntegerVector, vector<int>>(v4_vec);
    vector<int> v6(3);
    v6 << 1, 2, 3;
    expect_equal_vector<vector<int>>(v4_vec, v6);
    expect_equal_vector<IntegerVector>(v4, v5);

    NumericVector v_m = NumericVector::create(1.0, 2.0, 3.0, 4.0);
    NumericMatrix m1(2, 2, v_m.begin());
    matrix<double> m2(2, 2);
    m2 << 1.0, 3.0, 2.0, 4.0;
    expect_equal_matrix(m2, as_matrix<matrix<double>, NumericMatrix>(m1));
    expect_equal_matrix(m1, as_matrix<NumericMatrix, matrix<double>>(m2));
  }
}

context("segment works for Rcpp Vector") {
  test_that("segment have correct values") {
    NumericVector v1 = NumericVector::create(1.0, 2.0, 3.0);
    NumericVector v2 = segment<NumericVector>(v1, 1, 1);
    NumericVector v3 = NumericVector::create(2.0);
    expect_equal_vector(v2, v3);
  }
}
