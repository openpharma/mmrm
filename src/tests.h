#ifndef TESTS_INCLUDED_
#define TESTS_INCLUDED_

#include "testthat-helpers.h"
#include "covariance.h"
#include "utils.h"

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
    corr_fun_compound_symmetry<double> test_fun(theta);
    expect_equal(test_fun(1, 0), 0.7682213);
    expect_equal(test_fun(4, 1), 0.7682213);
    expect_equal(test_fun(3, 1), 0.7682213);
  }

  test_that("get_compound_symmetry produces expected result") {
    vector<double> theta {{log(2.0), 3.0}};
    matrix<double> result = get_compound_symmetry(theta, 3);
    matrix<double> expected(3, 3);
    expected <<
      2, 0, 0,
      1.89736659610103, 0.632455532033676, 0,
      1.89736659610103, 0.307900211696917, 0.552446793489648;
    expect_equal_matrix(result, expected);
  }

  test_that("get_compound_symmetry_heterogeneous produces expected result") {
    vector<double> theta {{log(1.0), log(2.0), log(3.0), 2.0}};
    matrix<double> result = get_compound_symmetry_heterogeneous(theta, 3);
    matrix<double> expected(3, 3);
    expected <<
      1, 0, 0,
      1.78885438199983, 0.894427190999916, 0,
      2.68328157299975, 0.633436854000505, 1.18269089452568;
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

context("get_cov_lower_chol_grouped") {
  test_that("get_cov_lower_chol_grouped gives expected unstructured result") {
    vector<double> theta {{log(1.0), log(2.0), 3.0, log(4.0), log(5.0), 6.0}};
    matrix<double> result = get_cov_lower_chol_grouped(theta, 2, "us", 2, false);
    matrix<double> expected(4, 2);
    expected << 1, 0, 6, 2, 4, 0, 30, 5;
    expect_equal_matrix(expected, result);
  }
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

#endif
