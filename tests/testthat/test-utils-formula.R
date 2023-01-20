
# # h_mmrm_tmb_exract_terms ---
# test_that("h_mmrm_tmb_extract_terms works for covariance terms as expected", {
#   expect_identical(
#     h_mmrm_tmb_extract_terms(quote(a | b)),
#     list(subject_var = "b", visit_var = "a", group_var = NULL)
#   )
#   expect_error(
#     h_mmrm_tmb_extract_terms(quote(a + b | b)),
#     "`time` in `time|\\(group/\\)subject` must be specified as one single variable."
#   )
#   expect_error(
#     h_mmrm_tmb_extract_terms(quote(a | (b + c))),
#     "Covariance structure must be of the form `time|\\(group/\\)subject`."
#   )
#   expect_identical(
#     h_mmrm_tmb_extract_terms(quote(a | b / c)),
#     list(subject_var = "c", visit_var = "a", group_var = "b")
#   )
#   expect_error(
#     h_mmrm_tmb_extract_terms(quote(a | (b + d) / c)),
#     "`group` in `time|\\(group/\\)subject` must be specified as one single variable."
#   )
#   expect_error(
#     h_mmrm_tmb_extract_terms(quote(a | b / (c + d))),
#     "`subject` in `time|\\(group/\\)subject` must be specified as one single variable."
#   )
# })



# # h_mmrm_tmb_extract_vars ----
# test_that("h_mmrm_tmb_extract_vars works for non-grouped formula as expected", {
#   expect_identical(
#     h_mmrm_tmb_extract_vars(quote(cs(a | b))),
#     list(subject_var = "b", visit_var = "a", group_var = NULL, is_spatial = FALSE)
#   )
#   expect_identical(
#     h_mmrm_tmb_extract_vars(quote(sp_exp(a1, a2 | b))),
#     list(subject_var = "b", visit_var = c("a1", "a2"), group_var = NULL, is_spatial = TRUE)
#   )
#   expect_error(
#     h_mmrm_tmb_extract_vars(quote(cs(a + b))),
#     "Covariance structure must be of the form `time|\\(group/\\)subject`."
#   )
# })

# test_that("h_mmrm_tmb_extract_vars works for grouped formula as expected", {
#   expect_identical(
#     h_mmrm_tmb_extract_vars(quote(cs(a | b / c))),
#     list(subject_var = "c", visit_var = "a", group_var = "b", is_spatial = FALSE)
#   )
#   expect_error(
#     h_mmrm_tmb_extract_vars(quote(cs((a + b) | c / d))),
#     "`time` in `time|\\(group/\\)subject` must be specified as one single variable."
#   )
#   expect_error(
#     h_mmrm_tmb_extract_vars(quote(cs(a | b / (c + d)))),
#     "`subject` in `time|\\(group/\\)subject` must be specified as one single variable."
#   )
#   expect_error(
#     h_mmrm_tmb_extract_vars(quote(cs(a | (b + c) / d))),
#     "`group` in `time|\\(group/\\)subject` must be specified as one single variable."
#   )
# })


# test_that("h_mmrm_tmb_extract_vars works for multiple coordinates as expected", {
#   expect_identical(
#     h_mmrm_tmb_extract_vars(quote(sp_exp(a1, a2, a3 | b))),
#     list(subject_var = "b", visit_var = c("a1", "a2", "a3"), group_var = NULL, is_spatial = TRUE)
#   )
#   expect_error(
#     h_mmrm_tmb_extract_vars(quote(us(a1, a2, a3 | b))),
#     "Non-spatial covariance term should not include multiple `time` variables."
#   )
#   expect_warning(
#     h_mmrm_tmb_extract_vars(quote(sp_exp(a1, a1 | b))),
#     "Duplicated `time` variable spotted: a1. This may indicate input errors in the formula."
#   )
# })
