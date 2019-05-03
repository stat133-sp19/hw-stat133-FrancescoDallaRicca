context("check checker functions")

test_that("check_prob", {
  expect_true(check_prob(0.5))
  expect_error(check_prob(2))
  expect_equivalent(check_prob(0.5), check_prob(0.6))
})

test_that("check_trials", {
  expect_true(check_trials(1))
  expect_error(check_trials(1.1))
  expect_equivalent(check_trials(1), check_trials(2))
})

test_that("check_success", {
  expect_true(check_success(3, 4))
  expect_false(any(check_success(c(1, 2, 3, 4), 5) == FALSE))
  expect_error(check_success(6, 5))
})
