library(phylosamp)
library(testthat)

context("prob_trans_mtml")

test_that("returns 1 when sensitivity perfect", {
  
  expect_that(
    prob_trans_mtml(eta=1, 
                    chi=1, 
                    rho=1, 
                    M=10,
                    R=1), 
    equals(1)
  )
  
  expect_that(
    prob_trans_mtml(eta=1, 
                    chi=1, 
                    rho=0.5, 
                    M=10,
                    R=1), 
    equals(1)
  )
})

test_that("returns 0 when sensitivity 0", {
  
  expect_that(
    prob_trans_mtml(eta=0, 
                    chi=0.99, 
                    rho=1, 
                    M=10,
                    R=1), 
    equals(0)
  )
  
  expect_that(
    prob_trans_mtml(eta=0, 
                    chi=0.99, 
                    rho=0.5, 
                    M=10,
                    R=1), 
    equals(0)
  )
  
  expect_that(
    prob_trans_mtml(eta=0, 
                    chi=0.5, 
                    rho=1, 
                    M=10,
                    R=1), 
    equals(0)
  )
})

test_that("fails when parameters invalid", {
  
  expect_error(
    prob_trans_mtml(eta=5, 
                    chi=1, 
                    rho=1, 
                    M=10,
                    R=1)
  )
  
  expect_error(
    prob_trans_mtml(eta=1, 
                    chi=5, 
                    rho=0.5, 
                    M=10,
                    R=1)
  )
  
  expect_error(
    prob_trans_mtml(eta=1, 
                    chi=1, 
                    rho=5, 
                    M=10,
                    R=1)
  )
  
  expect_error(
    prob_trans_mtml(eta=1, 
                    chi=1, 
                    rho=0.5, 
                    M=-1,
                    R=1)
  )
  
  expect_error(
    prob_trans_mtml(eta=0.99, 
                    chi=0.99, 
                    rho=0, 
                    M=10,
                    R=1)
  )
  
  expect_warning(
    prob_trans_mtml(eta=0.99, 
                    chi=0.99, 
                    rho=0.5, 
                    M=10,
                    R=2)
  )
})