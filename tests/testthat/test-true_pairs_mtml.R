library(phylosamp)
library(testthat)

context("true_pairs_mtml")

test_that("returns M*rho when sensitivity perfect", {
  
  expect_that(
    true_pairs_mtml(eta=1, 
                    rho=1, 
                    M=10,
                    R=1), 
    equals(10)
  )
  
  expect_that(
    true_pairs_mtml(eta=1, 
                    rho=0.5, 
                    M=10,
                    R=1), 
    equals(5)
  )
})

test_that("returns 0 when sensitivity 0", {
  
  expect_that(
    true_pairs_mtml(eta=0, 
                    rho=1, 
                    M=10,
                    R=1), 
    equals(0)
  )
  
})

test_that("fails when parameters invalid", {
  
  expect_error(
    true_pairs_mtml(eta=5, 
                    rho=1, 
                    M=10,
                    R=1)
  )
  
  expect_error(
    true_pairs_mtml(eta=1, 
                    rho=5, 
                    M=10,
                    R=1)
  )
  
  expect_error(
    true_pairs_mtml(eta=1, 
                    rho=0.5, 
                    M=-1,
                    R=1)
  )
  
  expect_error(
    true_pairs_mtml(eta=0.99, 
                    rho=0, 
                    M=10,
                    R=1)
  )
  
  expect_warning(
    true_pairs_mtml(eta=0.99, 
                    rho=0.5, 
                    M=10,
                    R=2)
  )
})