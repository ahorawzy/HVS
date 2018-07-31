library(HVS)
testthat::context("test for caculate_natureflow")

df <- data.frame(a=1:3,b=letters[1:3])
df$c[[1]] <- extract_link("1,3,5,7,8")
df$c[[2]] <- extract_link("1,4,6,8")
df$c[[3]] <- extract_link("1,4,5,7,9")

testthat::test_that("base function can work well",{
  testthat::expect_equal(caculate_natureflow("5-7",df$c),2)
  testthat::expect_equal(caculate_natureflow("1-4",df$c),2)
  testthat::expect_equal(caculate_natureflow("7-8",df$c),1)
})
