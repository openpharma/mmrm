#include "testthat-helpers.h"
#include "utils.h"
using namespace Rcpp;

context("Rcpp and eigen conversion") {
  test_that("conversions does not change values") {
    NumericVector v1 = NumericVector::create(1.0, 2.0, 3.0);
    auto v1_vec = as_vector(v1);
    vector<double> v2(3);
    v2 << 1.0, 2.0, 3.0;
    expect_equal_vector(v1_vec, v2);
    expect_equal_vector(v1_vec, as_vector(as_nv(v2)));

    IntegerVector v3 = IntegerVector::create(1, 2, 3);
    auto v3_vec = as_vector(v3);
    vector<int> v4(3);
    v4 << 1, 2, 3;
    expect_equal_vector(v3_vec, v4);
    
    NumericVector v_m = NumericVector::create(1.0, 2.0, 3.0, 4.0);
    NumericMatrix m1(2, 2, v_m.begin());
    matrix<double> m2(2, 2);
    m2 << 1.0, 3.0, 2.0, 4.0;
    expect_equal_matrix(m2, as_matrix(m1));
    expect_equal_matrix(m2, as_matrix(as_mv(as_matrix(m1))));
  }
}
