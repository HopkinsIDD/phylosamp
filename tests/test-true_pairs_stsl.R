library(phylosamp)
library(testthat)

context("true_pairs_stsl")

test_that("returns (M/2) * rho when sensitivity perfect", {
  
  expect_that(
    true_pairs_stsl(eta=1, 
                    rho=1, 
                    M=10), 
    equals(5)
  )
  
  expect_that(
    true_pairs_stsl(eta=1, 
                    rho=0.5, 
                    M=10), 
    equals(2.5)
  )
})

test_that("returns 0 when sensitivity 0", {
  
  expect_that(
    true_pairs_stsl(eta=0, 
                    rho=1, 
                    M=10), 
    equals(0)
  )

})


test_that("fails when parameters invalid", {
  
  expect_error(
    true_pairs_stsl(eta=5, 
                    rho=1, 
                    M=10)
  )
   
  expect_error(
    true_pairs_stsl(eta=1, 
                    rho=5, 
                    M=10)
  )
  
  expect_error(
    true_pairs_stsl(eta=1, 
                    rho=0.5, 
                    M=0)
  )
  
  expect_error(
    true_pairs_stsl(eta=0.99, 
                    rho=0, 
                    M=10)
  )
})