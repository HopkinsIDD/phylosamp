library(phylosamp)
library(testthat)

context("true_pairs")

test_that("returns M*rho or M/2 * rho when sensitivity perfect", {
  
  expect_that(
    true_pairs(eta=1, 
              rho=1, 
              M=10,
              R=NULL,
              assumption='stsl'), 
    equals(5)
  )
  
  expect_that(
    true_pairs(eta=1, 
              rho=1, 
              M=10,
              R=1,
              assumption='mtsl'), 
    equals(10)
  )
  
  expect_that(
    true_pairs(eta=1, 
              rho=1, 
              M=10,
              R=1,
              assumption='mtml'), 
    equals(10)
  )
})

test_that("fails when parameters invalid", {
  
  expect_error(
    true_pairs(eta=1, 
              rho=1, 
              M=10,
              R=1,
              assumption='bananas')
  )
  
})