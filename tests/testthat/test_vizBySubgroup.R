library(hetviz)

context("Data munging for 'Viz By Subgroup' tabs")

# expected constants to compare function output to
TOL <- 10^-3

test_that("collapseProp() gives proportions in each cell of 2x2 table", {
  expect_equal(collapseProp(data.frame(trt=simpleDataA$trt, simpleDataA$E1))$propor,
               c(0.240, 0.253, 0.241, 0.267),
               tolerance = TOL)
  expect_equal(collapseProp(data.frame(trt=simpleDataB$trt, simpleDataB$E1))$propor,
               c(0.267, 0.231, 0.255, 0.246),
               tolerance = TOL)
  expect_equal(collapseProp(data.frame(trt=simpleDataC$trt, simpleDataC$E1))$propor,
               c(0.250, 0.270, 0.246, 0.234),
               tolerance = TOL)
  expect_equal(collapseProp(data.frame(trt=simpleDataD$trt, simpleDataD$E1))$propor,
               c(0.163, 0.161, 0.325, 0.351),
               tolerance = TOL)
})
