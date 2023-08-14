# flatten_expr ----

test_that("flatten_expr collapses expressions into sequential atoms and ops", {
  expect_identical(
    flatten_expr(quote(1 + 2 | 3 * 4^5)),
    list(1, as.name("+"), 2, as.name("|"), 3, as.name("*"), 4, as.name("^"), 5)
  )

  expect_identical(
    flatten_expr(quote("a b c" * easy_as(1, 2, 3))),
    list("a b c", as.name("*"), as.call(quote(easy_as(1, 2, 3))))
  )
})

# flatten_call ----

test_that("flatten_call collapses expressions into sequential atoms and ops", {
  expect_identical(
    flatten_call(quote(us(1 + 2 | 3))),
    list(as.symbol("us"), 1, as.name("+"), 2, as.name("|"), 3)
  )

  expect_identical(
    flatten_call(quote(sp_exp(1, 2, 3, 4 | 5))),
    list(as.symbol("sp_exp"), 1, 2, 3, 4, as.name("|"), 5)
  )
})

# position_symbol ----

test_that("position_symbol returns position of a symbol in a list of language", {
  expect_identical(
    position_symbol(flatten_expr(quote(1 + 2 | 3)), "|"),
    4L
  )

  expect_identical(
    position_symbol(flatten_expr(quote(1 + 2 | 3 | 4)), "|"),
    4L
  )

  expect_identical(
    position_symbol(flatten_expr(quote(1 + 2 | 3 | 4)), "^", nomatch = Inf),
    Inf
  )

  expect_identical(
    position_symbol(flatten_expr(quote(1)), "=^..^=", nomatch = "no cat"),
    "no cat"
  )
})

# is_infix ----

test_that("is_infix identifies primitive infix operators", {
  expect_true(is_infix("+"))
  expect_true(is_infix(as.symbol("+")))
  expect_true(is_infix("%/%"))
  expect_true(is_infix("%%"))
  expect_false(is_infix("%special%"))
  expect_false(is_infix("paste"))
})

# formula_rhs ----

test_that("formula_rhs fetches the right-most part of a formula", {
  expect_identical(
    formula_rhs(a ~ b + c),
    ~ b + c,
  )

  expect_identical(
    formula_rhs(a + b ~ c),
    ~c
  )

  expect_identical(
    formula_rhs(~ c | d),
    ~ c | d
  )
})

# format_symbols ----

test_that("format_symbols formats variables as though they were passed to a formula", {
  # typical variables print as-is
  expect_identical(
    format_symbols(c("AVISITN", "USUBJID")),
    "AVISITN, USUBJID"
  )

  # variables with spaces or non-standard characters are printed in backticks
  expect_identical(
    format_symbols(c("VISIT DAY", "SUBJ-ID")),
    "`VISIT DAY`, `SUBJ-ID`"
  )

  # if some poor soul works with data that has backticks in the variable names,
  # they're covered too
  expect_identical(
    format_symbols(c("VISIT` DAY", "SUBJ-ID")),
    "`VISIT\\` DAY`, `SUBJ-ID`"
  )
})
