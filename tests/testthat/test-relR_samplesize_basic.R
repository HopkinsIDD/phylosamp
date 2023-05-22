test_that("Function errors properly if k is impossible", {
  expect_error(relR_samplesize_basic(1.5, 2, 0.5, 1000, overdispersion = 100))
})


test_that("Function errors properly without overdispersion", {
  R_a <- 1.5
  R_b <- 2
  d <- 1
  p_a <- 0.5
  N <- 1000
  m <- 1/2 * (sqrt(4 * (1 + R_a / (R_b * d^2)) *
                (stats::qnorm(1 - 0.025) + stats::qnorm(0.8))^2 * (N - 1) /
                (log(R_b/R_a)^2 * R_a * p_a^2) + 1) + 1)
  expect_equal(m, relR_samplesize_basic(1.5, 2, 0.5, 1000))
})

test_that("Function works with overdispersion", {
  m <- relR_samplesize_basic(1.5, 2, 0.5, 1000, overdispersion = 350)
  expect_equal(round(m), 983)
})

test_that("Function works with overdispersion set to Inf", {
  m1 <- relR_samplesize_basic(1.5, 2, 0.5, 1000)
  m2 <- relR_samplesize_basic(1.5, 2, 0.5, 1000, overdispersion = Inf)
  expect_equal(m1, m2)
})
