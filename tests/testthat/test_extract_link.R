library(HVS)
context("test for extract_link")

testthat::test_that("base function can work well",{
  el <- extract_link("1,3,5,7,8")
  testthat::expect_equal(sum(el == c("1-3","3-5","5-7","7-8")),4)
})
