#include "testthat-helpers.h"
#include "derivatives.h"
#include <iostream>

context("cho_jacobian") {
  test_that("cho_jacobian works as expected") {
    chol_jacobian chol_jac_obj(2, "ar1");
    vector<double> theta {{1.0, 1.0}};
    vector<double> result = chol_jac_obj(theta);
    vector<double> expected(8);
    // expected obtained from numDeriv::jacobian and ar1 sigma
    expected << 2.718282, 1.922116, 0, 1.922116, 0.0, 0.9610578, 0.0, -0.9610578;
    expect_equal_vector(result, expected);
  }
  test_that("cho_jacobian's jacabian using autodiff works as expected") {
    chol_jacobian chol_jac_obj(2, "ar1");
    vector<double> theta {{1.0, 1.0}};
    vector<double> result = autodiff::jacobian(chol_jac_obj,theta).vec();
    vector<double> expected(16);
    // expected obtained from two numDeriv::jacobian and ar1 sigma
    expected << 2.718282, 1.9221164, 0, 1.9221167, 0.0, 0.9610586, 0.0, -0.9610586, 0.0, 0.9610586, 0.0, -0.9610586, 0.0, -1.4415871, 0.0, 0.4805284;
    expect_equal_vector(result, expected);
  }
}

context("derivatives_nonspatial struct works as expected") {
  test_that("derivatives_nonspatial struct correct sigma, inverse and derivatives") {
    vector<double> theta {{1.0, 1.0}};
    auto mychol = derivatives_nonspatial<double>(theta, 4, "ar1");
    std::vector<int> v1 {0, 1, 2};
    std::vector<int> v_full {0, 1, 2, 3};
    matrix<double> dist(0, 0);
    auto full_sigma = mychol.get_sigma(v_full, dist);
    auto part_sigma = mychol.get_sigma(v1, dist);
    auto full_inverse = matrix<double>(mychol.get_sigma_inverse(v_full, dist));
    matrix<double> expected_inverse(4, 4);
    // expected values from R side solve
    expected_inverse << 0.2706706, -0.191393, 0, 0, -0.191393, 0.4060058, -0.191393, 0, 0, -0.191393, 0.4060058, -0.191393, 0,0,-0.191393, 0.2706706;
    expect_equal_matrix(expected_inverse, full_inverse);

    auto v1_inverse = matrix<double>(mychol.get_sigma_inverse(v1, dist));
    matrix<double> expected_v1_inverse(3, 3);
    // expected values from R side solve
    expected_v1_inverse <<
        0.270670566473225, -0.191392993020822, 0,
        -0.191392993020822, 0.406005849709838, -0.191392993020822,
        0, -0.191392993020822, 0.270670566473225;
    expect_equal_matrix(expected_v1_inverse, v1_inverse);

    auto derivative1 = mychol.get_sigma_derivative1(v1, dist);
    matrix<double> expected_derivative1(3, 3);
    // expected values from R side numDeriv::jacobian
    expected_derivative1 << 
        14.7781121978613, 10.4497033482434, 7.38905609893065,
        10.4497033482434, 14.7781121978613, 10.4497033482434,
        7.38905609893065, 10.4497033482434, 14.7781121978613;
    expect_equal_matrix(matrix<double>(derivative1.block(0, 0, 3, 3)), expected_derivative1);

    auto derivative2 = mychol.get_sigma_derivative2(v1, dist);
    matrix<double> expected_derivative2(3, 3);
    // expected values from R side two numDeriv::jacobian
    expected_derivative2 << 
        29.5562243957226, 20.8994066964867, 14.7781121978613,
        20.8994066964867, 29.5562243957226, 20.8994066964867,
        14.7781121978613, 20.8994066964867, 29.5562243957226;
    expect_equal_matrix(matrix<double>(derivative2.block(0, 0, 3, 3)), expected_derivative2);
    auto inverse_derivative = mychol.get_inverse_derivative(v1, dist);
    expect_equal_matrix(matrix<double>(inverse_derivative.block(0, 0, 3, 3)), matrix<double>(- v1_inverse * derivative1.block(0, 0, 3, 3) * v1_inverse));
  }
}

context("derivatives_sp_exp struct works as expected") {
  test_that("derivatives_sp_exp struct gives correct sigma, inverse and derivatives") {
    vector<double> theta {{1.0, 1.0}};
    auto sp = derivatives_sp_exp<double>(theta, "sp_exp");
    matrix<double> dist (3, 3);
    dist << 
      0, 0.5, 1,
      0.5, 0, 0.5,
      1, 0.5, 0;
    std::vector<int> v(0);
    auto sigma = sp.get_sigma(v, dist);
    matrix<double> expected_sigma(3, 3);
    // expected values from R side two rho^dist * sigma
    expected_sigma << 
      2.718282, 2.324184, 1.987223,
      2.324184, 2.718282, 2.324184,
      1.987223, 2.324184, 2.718282;
    expect_equal_matrix(sigma, expected_sigma);

    auto sigma_d1 = sp.get_sigma_derivative1(v, dist);
    matrix<double> expected_sigma_d1(6, 3);
    // expected values from R side numDeriv::jacobian
    expected_sigma_d1 << 
      2.71828182844263, 2.32418434058079, 1.9872232498215,
      2.32418434058079, 2.71828182844263, 2.32418434058079,
      1.9872232498215, 2.32418434058079, 2.71828182844263,
      0, 0.312534720067585, 0.534446645412701,
      0.312534720067585, 0, 0.312534720067585,
      0.534446645412701, 0.312534720067585, 0;
    expect_equal_matrix(sigma_d1, expected_sigma_d1);

    auto sigma_d2 = sp.get_sigma_derivative2(v, dist);
    matrix<double> expected_sigma_d2(12, 3);
    // expected values from R side two times numDeriv::jacobian
    expected_sigma_d2 << 
      2.718281070007, 2.32418298874968, 1.98722393345662,
      2.32418298874968, 2.718281070007, 2.32418298874968,
      1.98722393345662, 2.32418298874968, 2.718281070007,
      0, 0.312537183788863, 0.534447011054242,
      0.312537183788863, 0, 0.312537183788863,
      0.534447011054242, 0.312537183788863, 0,
      0, 0.312537183793268, 0.53444701104616,
      0.312537183793268, 0, 0.312537183793268,
      0.53444701104616, 0.312537183793268, 0,
      0, -0.1864537925375, -0.246976228442905,
      -0.1864537925375, 0, -0.1864537925375,
      -0.246976228442905, -0.1864537925375, 0;
    expect_equal_matrix(sigma_d2, expected_sigma_d2);

    auto sigma_inv = sp.get_sigma_inverse(v, dist);
    matrix<double> expected_sigma_inv(3, 3);
    // expected values from R side use solve
    expected_sigma_inv << 
      1.367879, -1.169564, 0,
      -1.169564, 2.367879, -1.169564,
      0, -1.169564,  1.367879;
    expect_equal_matrix(sigma_inv, expected_sigma_inv);
  }
}