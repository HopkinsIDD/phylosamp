library(phylosamp)
library(testthat)

context("sens_spec_calc")

test_that("sensitivity estimate found", {
  
  expect_that(
    sens_spec_calc(cutoff=2, 
                   mut_rate=1, 
                   mean_gens_pdf=c(0.02,0.08,0.15,0.75), 
                   max_link_gens=1)[1,"sensitivity"][[1]], 
    equals(0.7357589)
  )
})

test_that("specificity estimate found", {
  
  expect_that(
    sens_spec_calc(cutoff=2, 
                   mut_rate=1, 
                   mean_gens_pdf=c(0.02,0.08,0.15,0.75), 
                   max_link_gens=1)[1,"specificity"][[1]], 
    equals(0.8662894)
  )
})

test_that("returns sensitivity and specificity estimates for a range of cutoff values", {
  
  expect_that(
    dim(sens_spec_calc(cutoff=10, 
                       mut_rate=1, 
                       mean_gens_pdf=c(0.02,0.08,0.15,0.75), 
                       max_link_gens=1))[1], 
    equals(10)
  )
})