context("check summary functions")

test_that("aux_mean", {
  expect_true(is.numeric(aux_mean(1, 0.5)))
  expect_equal(aux_mean(1, 0.5), 0.5)
  expect_is(aux_mean(c(1, 2, 3, 4), 0.5), "numeric")
})

test_that("aux_variance", {
  expect_true(is.numeric(aux_variance(1, 0.5)))
  expect_equal(aux_variance(10, 0.3), 2.1)
  expect_is(aux_variance(c(1, 2, 3, 4), 0.5), "numeric")
})

test_that("aux_mode", {
  expect_true(is.numeric(aux_mean(1, 0.5)))
  expect_equal(aux_mode(10, 0.3), 3)
  expect_type(aux_mode(10, 0.5), "double")
})

test_that("aux_skewness", {
  expect_true(is.numeric(aux_skewness(1, 0.5)))
  expect_equal(aux_skewness(10, 0.3), ((1-2*0.3)/(sqrt(10*0.3*(1-0.3)))))
  expect_is(aux_skewness(c(1, 2, 3, 4), 0.5), "numeric")
})

test_that("aux_kurtosis", {
  expect_true(is.numeric(aux_kurtosis(1, 0.5)))
  expect_equal(aux_kurtosis(10, 0.3), ((1-(6*0.3*(1-0.3)))/(10*0.3*(1-0.3))))
  expect_is(aux_kurtosis(c(1, 2, 3, 4), 0.5), "numeric")
})
