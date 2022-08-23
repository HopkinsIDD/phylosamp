library(phylosamp)
library(testthat)

context("obs_pairs_stsl")

test_that("returns (M/2) * rho when sensitivity perfect", {
  
  expect_that(
    obs_pairs_stsl(eta=1, 
                    chi=1, 
                    rho=1, 
                    M=10), 
    equals(5)
  )
  
  expect_that(
    obs_pairs_stsl(eta=1, 
                    chi=1, 
                    rho=0.5, 
                    M=10), 
    equals(2.5)
  )
})

test_that("fails when parameters invalid", {
  
  expect_error(
    obs_pairs_stsl(eta=5, 
                    chi=1, 
                    rho=1, 
                    M=10)
  )
  
  expect_error(
    obs_pairs_stsl(eta=1, 
                    chi=5, 
                    rho=0.5, 
                    M=10)
  )
  
  expect_error(
    obs_pairs_stsl(eta=1, 
                    chi=1, 
                    rho=5, 
                    M=10)
  )
  
  expect_error(
    obs_pairs_stsl(eta=1, 
                    chi=1, 
                    rho=0.5, 
                    M=-1)
  )
  
  expect_error(
    obs_pairs_stsl(eta=0.99, 
                    chi=0.99, 
                    rho=0, 
                    M=10)
  )
})