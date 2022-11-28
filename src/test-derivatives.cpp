#include <Rcpp.h>
#define INCLUDE_RCPP
#include "utils.h"
#include "covariance.h"
#include "chol_derivative.h"
#include "testthat-helpers.h"
#include <iostream>

context("cho_jacobian") {
  test_that("cho_jacobian works as expected") {
    chol_jacobian chol_jac_obj(2, "ar1");
    vector<double> theta {{1.0, 1.0}};
    vector<double> result = chol_jac_obj(theta);
    vector<double> expected(8);
    expected << 2.718282, 1.922116, 0, 1.922116, 0.0, 0.9610578, 0.0, -0.9610578;
    expect_equal_vector(result, expected);
  }
  test_that("cho_jacobian's jacabian using autodiff works as expected") {
    chol_jacobian chol_jac_obj(2, "ar1");
    vector<double> theta {{1.0, 1.0}};
    vector<double> result = autodiff::jacobian(chol_jac_obj,theta).vec();
    vector<double> expected(16);
    expected << 2.718282, 1.9221164, 0, 1.9221167, 0.0, 0.9610586, 0.0, -0.9610586, 0.0, 0.9610586, 0.0, -0.9610586, 0.0, -1.4415871, 0.0, 0.4805284;
    expect_equal_vector(result, expected);
  }
}

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

context("chols struct works as expected") {
  test_that("chols struct correct sigma, inverse and derivatives") {
    vector<double> theta {{1.0, 1.0}};
    auto mychol = chols<double>(theta, 4, "ar1");
    std::vector<int> v1 {0, 1, 2};
    std::vector<int> v_full {0, 1, 2, 3};
    auto full_sigma = mychol.get_sigma(v_full);
    auto part_sigma = mychol.get_sigma(v1);
    auto selmat = get_select_matrix<double>(v1, 4);
    expect_equal_matrix(matrix<double>(selmat), matrix<double>(mychol.get_sel_mat(v1)));
    expect_equal_matrix(matrix<double>(selmat * full_sigma.block(0,0,4,4) * selmat.transpose()), matrix<double>(part_sigma.block(0,0,3,3)));
    auto full_inverse = matrix<double>(mychol.get_inverse(v_full));
    matrix<double> expected_inverse(4, 4);
    expected_inverse << 0.2706706, -0.191393, 0, 0, -0.191393, 0.4060058, -0.191393, 0, 0, -0.191393, 0.4060058, -0.191393, 0,0,-0.191393, 0.2706706;
    expect_equal_matrix(expected_inverse, full_inverse);

    auto v1_inverse = matrix<double>(mychol.get_inverse(v1));
    matrix<double> expected_v1_inverse(3, 3);
    expected_v1_inverse <<
        0.270670566473225, -0.191392993020822, 0,
        -0.191392993020822, 0.406005849709838, -0.191392993020822,
        0, -0.191392993020822, 0.270670566473225;
    expect_equal_matrix(expected_v1_inverse, v1_inverse);

    auto derivative1 = mychol.get_sigma_derivative1(v1);
    matrix<double> expected_derivative1(3, 3);
    expected_derivative1 << 
        14.7781121978613, 10.4497033482434, 7.38905609893065,
        10.4497033482434, 14.7781121978613, 10.4497033482434,
        7.38905609893065, 10.4497033482434, 14.7781121978613;
    expect_equal_matrix(matrix<double>(derivative1.block(0, 0, 3, 3)), expected_derivative1);

    auto derivative2 = mychol.get_sigma_derivative2(v1);
    matrix<double> expected_derivative2(3, 3);
    expected_derivative2 << 
        29.5562243957226, 20.8994066964867, 14.7781121978613,
        20.8994066964867, 29.5562243957226, 20.8994066964867,
        14.7781121978613, 20.8994066964867, 29.5562243957226;
    expect_equal_matrix(matrix<double>(derivative2.block(0, 0, 3, 3)), expected_derivative2);
    auto inverse_derivative = mychol.get_inverse_derivative(v1);
    expect_equal_matrix(matrix<double>(inverse_derivative.block(0, 0, 3, 3)), matrix<double>(- v1_inverse * derivative1.block(0, 0, 3, 3) * v1_inverse));
  }
}
