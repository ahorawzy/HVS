library(HVS)
testthat::context("test for all_link")

A = matrix(c(0,2,6,0,2,0,5,7,6,5,0,1,0,7,1,0),nrow=4)
rownames(A) <- 11:14
colnames(A) <- 11:14

testthat::test_that("base funtion can work well",{
  al <- all_link(A)
  testthat::expect_equal(sum(al == c("11-12","11-13","12-11","12-13",
                                     "12-14","13-11","13-12","13-14",
                                     "14-12","14-13")),10)
})
