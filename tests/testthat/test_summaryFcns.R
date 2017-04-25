library(hetviz)

TOL <- 1e-3

context("Summary functions are summarizing correctly")

test_that("expit() produces the correct value", {
  expect_equal(expit(5),
               0.9933071,
               tolerance = TOL)
  expect_equal(expit(0.5),
               0.6224593,
               tolerance = TOL)
})
