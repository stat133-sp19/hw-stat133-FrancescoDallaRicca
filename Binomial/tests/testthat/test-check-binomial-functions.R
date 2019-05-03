context("check binomial functions")

test_that("bin_choose", {
  expect_error(bin_choose(1, 2))
  expect_error(bin_choose(2, c(1, 2, 3)))
  expect_equal(bin_choose(2, 1), 2)
})

test_that("bin_probability", {
  expect_error(bin_probability(2, 1, 0.5))
  expect_error(bin_probability(2, 4, 2))
  expect_error(bin_probability(2.1, 5, 2))
})

test_that("bin_distribution", {
  expect_true(any(class(bin_distribution(5, 0.5)) == "bindis"))
  expect_true(ncol(bin_distribution(5, 0.5)) == 2)
  expect_true(nrow(bin_distribution(5, 0.5)) == 6)
})

test_that("bin_cumulative", {
  expect_true(any(class(bin_cumulative(5, 0.5)) == "bincum"))
  expect_true(any(class(bin_cumulative(5, 0.5)) == "data.frame"))
  expect_true(nrow(bin_cumulative(5, 0.5)) == 6)
})
