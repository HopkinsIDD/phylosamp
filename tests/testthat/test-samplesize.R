library(phylosamp)
library(testthat)

context("samplesize")

test_that("fails when sensitivity 0", {
  
  expect_error(
    samplesize(eta=0,
               chi=0.995,
               N=100,
               R=1,
               phi=0.75)
  )

})


test_that("fails when parameters invalid", {
  
  expect_error(
    samplesize(eta=0.99,
               chi=0.995,
               N=100,
               R=1,
               phi=2)
  )
   
  expect_error(
    samplesize(eta=0.99,
               chi=0.995,
               N=100,
               R=1,
               phi=-2)
  )
})