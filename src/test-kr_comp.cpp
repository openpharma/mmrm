#include "testthat-helpers.h"
#include "kr_comp.h"

context("KR covariance directions") {
  test_that("directions reconstruct positive, semidefinite and indefinite W without truncation") {
    matrix<double> rotation(3, 3);
    rotation << 1, 2, -1, 0, 1, 2, 2, -1, 1;
    for (double last : {0.5, 0.0, -0.5, 1e-12}) {
      vector<double> values(3);
      values << 2, 1, last;
      matrix<double> w = rotation * values.matrix().asDiagonal() * rotation.transpose();
      kr_directions directions(w);
      matrix<double> reconstructed = directions.B * directions.signs.matrix().asDiagonal() * directions.B.transpose();
      expect_true(directions.B.cols() == 3);
      expect_true((reconstructed - w).norm() / w.norm() < 1e-12);
      if (last < 0) expect_true(directions.signs.minCoeff() == -1);
    }
  }
  test_that("pattern directions equal first-derivative combinations with no second derivatives") {
    vector<double> theta(3);
    theta << 0.1, 0.2, 0.4;
    derivatives_nonspatial<double> cache(theta, 2, "us", false);
    matrix<double> w = matrix<double>::Identity(3, 3);
    w(0, 1) = w(1, 0) = 0.3;
    kr_directions directions(w);
    matrix<double> dist(0, 0);
    for (std::vector<int> visits : {std::vector<int>{0, 1}, std::vector<int>{1}}) {
      kr_pattern pattern(&cache, visits, dist, directions.B);
      int m = visits.size();
      for (int ell = 0; ell < 3; ++ell) {
        matrix<double> expected = matrix<double>::Zero(m, m);
        for (int h = 0; h < 3; ++h) {
          expected += directions.B(h, ell) * pattern.inverse_d1.block(h * m, 0, m, m);
        }
        expect_true((expected - pattern.inverse_directions.block(ell * m, 0, m, m)).norm() < 1e-12);
      }
    }
    expect_true(cache.sigmad2_cache.empty());
  }
}
