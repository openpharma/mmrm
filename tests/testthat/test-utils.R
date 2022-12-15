# h_record_all_outputs ----

test_that("h_record_all_outputs correctly removes specified messages", {
  result <- h_record_all_output({
    x <- 1
    y <- 2
    warning("something went wrong")
    message("O nearly done")
    message("Almost done")
    x + y
  }, remove = list(messages = c("Almost done", "bla")))
  expected <- list(result = 3, warnings = "something went wrong", errors = NULL, messages = "O nearly done")
  expect_identical(result, expected)
})

test_that("h_record_all_outputs works as expected with no removal list given for messages", {
  result <- h_record_all_output({
    x <- 1
    y <- 2
    warning("something went wrong")
    message("O nearly done")
    message("oh noo")
    x + y
  })
  expected <- list(result = 3, warnings = "something went wrong", errors = NULL,
  messages = c("O nearly done", "oh noo"))
  expect_identical(result, expected)
})


# h_tr ----

test_that("trace of a matrix works as expected", {
  mx <- matrix(0, nrow = 3, ncol = 4)
  expect_error(h_tr(mx), "x must be square matrix")
  v <- c(1, 3, 2)
  expect_equal(h_tr(diag(v)), 6)
})

# free_cors ----

test_that("free_cores throws deprecation warning", {
  skip_on_cran()
  expect_warning(
    free_cores(),
    regexp = "`free_cores()` was deprecated in mmrm 0.1.6.",
    fixed = TRUE
  )
})

# h_split_control ----

test_that("h_split_control split the control args based on optimizers", {
  control <- mmrm_control()
  controls <- h_split_control(control)
  expect_identical(length(controls), length(control$optimizers))
  non_opt <- c("start", "accept_singular", "method", "n_cores")
  for (i in seq_len(length(controls))) {
    expect_identical(controls[[i]]$optimizers[[1]], control$optimizers[[i]])
    expect_identical(controls[[i]][non_opt], control[non_opt])
  }
})

test_that("h_split_control split the control args with updated arguments", {
  control <- mmrm_control()
  start <- c(1, 2, 3)
  method <- "Kenward-Roger"
  controls <- h_split_control(control, start = start, method = method)
  for (i in seq_len(length(controls))) {
    expect_identical(controls[[i]][["start"]], start)
    expect_identical(controls[[i]][["method"]], method)
  }
})

# h_get_optimizers ----

test_that("h_get_optimizers works for default optimizers", {
  opt1 <- h_get_optimizers("nlminb")
  expect_identical(opt1, h_optimizer_fun("nlminb"), ignore_attr = TRUE)
  expect_identical(opt1[[1]], stats::nlminb, ignore_attr = TRUE)
})

test_that("h_get_optimizers works added arguments", {
  opt1 <- h_get_optimizers("nlminb", optimizer_args = list(a = 1, b = 2))
  expect_identical(attr(opt1[[1]], "args"), list(control = list(), a = 1, b = 2))
})

test_that("h_get_optimizers works custom optimizer", {
  opt1 <- h_get_optimizers(optimizer_fun = silly_optimizer, optimizer_args = list(a = 1, b = 2))
  expect_identical(opt1[[1]], silly_optimizer, ignore_attr = TRUE)
  expect_identical(attr(opt1[[1]], "args"), list(control = list(), a = 1, b = 2))
})

# h_optimizer_fun ----

test_that("h_optimizer_fun return correct optimizer", {
  opts <- h_optimizer_fun()
  expect_identical(opts[[1]], stats::optim, ignore_attr = TRUE)
  expect_identical(opts[[2]], stats::optim, ignore_attr = TRUE)
  expect_identical(opts[[3]], stats::optim, ignore_attr = TRUE)
  expect_identical(opts[[4]], stats::nlminb, ignore_attr = TRUE)
  expect_identical(attr(opts[[1]], "args"), list(method = "L-BFGS-B"))
  expect_identical(attr(opts[[2]], "args"), list(method = "BFGS"))
  expect_identical(attr(opts[[3]], "args"), list(method = "CG"))
  expect_identical(attr(opts[[4]], "use_hessian"), TRUE)
})

# h_partial_fun_args ----

test_that("h_partial_fun_args works correctly to add attributes", {
  opt1 <- h_partial_fun_args(stats::optim, a = 1, b = 2, additional_attr = list(a = 1, b = 2))
  expect_identical(opt1, stats::optim, ignore_attr = TRUE)
  expect_identical(attr(opt1, "args"), list(a = 1, b = 2))
  expect_identical(attr(opt1, "a"), 1)
  expect_identical(attr(opt1, "b"), 2)
})
