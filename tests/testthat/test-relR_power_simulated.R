test_that("Function works under Poisson assumption", {
  p <- relR_power_simulated(665, 1.5, 2, 0.5, 1000)
  expect_equal(round(p, 1), 0.8)
})


test_that("Function works under Negative Binomial assumption", {
  p <- relR_power_simulated(983, 1.5, 2, 0.5, 1000, overdispersion = 350)
  expect_equal(round(p, 1), 0.8)
})

