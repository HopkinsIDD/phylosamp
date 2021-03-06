library(phylosamp)
library(testthat)

context("gen_dists")

test_that("returns linked_prob==1 when max_dist 0 and mean_gens_pdf[1]>0", {
  
  expect_that(
    gen_dists(mut_rate = 1,
              mean_gens_pdf = c(0.2,0.2,0.2,0.2,0.2),
              max_dist = 0)[,"linked_prob"][[1]], 
    equals(1)
  )
  
  expect_that(
    gen_dists(mut_rate = 2,
              mean_gens_pdf = c(1),
              max_dist = 0)[,"linked_prob"][[1]], 
    equals(1)
  )
})

test_that("fails when parameters invalid", {
  
  expect_error(
    gen_dists(mut_rate = -1, 
              mean_gens_pdf = c(0.5,0.5), 
              max_link_gens = 1)
  )
  
  expect_error(
    gen_dists(mut_rate = 1, 
              mean_gens_pdf = 0)
  )
  
  expect_error(
    gen_dists(mut_rate = 1, 
              mean_gens_pdf = c(0.2,-0.2,0.6))
  )
  
  expect_error(
    gen_dists(mut_rate = 1, 
              mean_gens_pdf = c(0.2,-0.2,0.6), 
              max_gens = 0)
  )
  
  expect_error(
    gen_dists(mut_rate = 1, 
              mean_gens_pdf = c(0.2,-0.2,0.6), 
              max_dist = -1)
  )
  
  expect_error(
    gen_dists(mut_rate = 1, 
              mean_gens_pdf = c(0.2,-0.2,0.6), 
              max_link_gens = 0)
  )
})