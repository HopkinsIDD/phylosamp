library(phylosamp)
library(testthat)

context("exp_links")

test_that("returns M*rho or M/2 * rho when sensitivity perfect", {
  
  expect_that(
    exp_links(eta=1, 
              chi=1, 
              rho=1, 
              M=10,
              R=NULL,
              assumption='stsl'), 
    equals(5)
  )
  
  expect_that(
    exp_links(eta=1, 
              chi=1, 
              rho=1, 
              M=10,
              R=1,
              assumption='mtsl'), 
    equals(10)
  )
  
  expect_that(
    exp_links(eta=1, 
              chi=1, 
              rho=1, 
              M=10,
              R=1,
              assumption='mtml'), 
    equals(10)
  )
})

test_that("fails when parameters invalid", {
  
  expect_error(
    exp_links(eta=1, 
              chi=1, 
              rho=1, 
              M=10,
              R=1,
              assumption='bananas')
  )
  
})