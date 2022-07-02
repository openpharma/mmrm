#include "testthat-helpers.h"
#include "utils.h"

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
