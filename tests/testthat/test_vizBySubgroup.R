library(hetviz)

context("Data munging for 'Viz By Subgroup' tabs")

# expected constants to compare function output to
TOL <- 10^-3

test_that("collapseProp() gives proportions in each cell of 2x2 table", {
  expect_equal(collapseProp(data.frame(simpleDataA$E1, trt=simpleDataA$trt))$propor,
               c(0.240, 0.253, 0.241, 0.267),
               tolerance = TOL)
  expect_equal(collapseProp(data.frame(simpleDataB$E1, trt=simpleDataB$trt))$propor,
               c(0.267, 0.231, 0.255, 0.246),
               tolerance = TOL)
  expect_equal(collapseProp(data.frame(simpleDataC$E1, trt=simpleDataC$trt))$propor,
               c(0.250, 0.270, 0.246, 0.234),
               tolerance = TOL)
  expect_equal(collapseProp(data.frame(simpleDataD$E1, trt=simpleDataD$trt))$propor,
               c(0.163, 0.161, 0.325, 0.351),
               tolerance = TOL)
})
