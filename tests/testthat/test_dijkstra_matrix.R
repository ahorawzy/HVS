library(HVS)
context("test for dijkstra_matrix")

A <- matrix(c(0,2,6,0,2,0,5,7,6,5,0,1,0,7,1,0),nrow=4)
result <- dijkstra_matrix(A)
mileage <- result[[1]]
path <- result[[2]]

test_that("output contains two certain element",{
  expect_equal(length(result),2)
  expect_is(mileage,"matrix")
  expect_is(path,"list")
})

test_that("output mileage is correct",{
  expect_equal(sum(mileage[1,] == c(0,2,6,7)), 4)
  expect_equal(sum(mileage[2,] == c(2,0,5,6)), 4)
  expect_equal(sum(mileage[3,] == c(6,5,0,1)), 4)
  expect_equal(sum(mileage[4,] == c(7,6,1,0)), 4)
})

test_that("output path is correct",{
  expect_true(path[[1]][[1]] == "1")
  expect_true(path[[1]][[2]] == "1,2")
  expect_true(path[[1]][[3]] == "1,3")
  expect_true(path[[1]][[4]] == "1,3,4")
  expect_true(path[[2]][[3]] == "2,3")
  expect_true(path[[3]][[4]] == "3,4")
})
