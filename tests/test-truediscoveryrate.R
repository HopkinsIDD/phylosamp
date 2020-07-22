library(phylosamp)
library(testthat)

context("truediscoveryrate")

test_that("returns 1 when sensitivity perfect", {
  
  expect_that(
    truediscoveryrate(eta=1, 
                      chi=1, 
                      rho=1, 
                      M=10,
                      R=NULL,
                      assumption='stsl'), 
    equals(1)
  )
  
  expect_that(
    truediscoveryrate(eta=1, 
                      chi=1, 
                      rho=1, 
                      M=10,
                      R=1,
                      assumption='mtsl'), 
    equals(1)
  )
  
  expect_that(
    truediscoveryrate(eta=1, 
                      chi=1, 
                      rho=1, 
                      M=10,
                      R=1,
                      assumption='mtml'), 
    equals(1)
  )
})


test_that("returns 0 when sensitivity 0", {
  
  expect_that(
    truediscoveryrate(eta=0, 
                      chi=0.99, 
                      rho=1, 
                      M=10,
                      R=NULL,
                      assumption='stsl'), 
    equals(0)
  )
  
  expect_that(
    truediscoveryrate(eta=0, 
                      chi=0.99, 
                      rho=1, 
                      M=10,
                      R=1,
                      assumption='mtsl'), 
    equals(0)
  )
  
  expect_that(
    truediscoveryrate(eta=0, 
                      chi=0.99, 
                      rho=1, 
                      M=10,
                      R=1,
                      assumption='mtml'), 
    equals(0)
  )
})

test_that("fails when parameters invalid", {
  
  expect_error(
    truediscoveryrate(eta=0.9,
                      chi=0.9,
                      rho=1, 
                      M=10,
                      R=1,
                      assumption="bananas")
  )
})