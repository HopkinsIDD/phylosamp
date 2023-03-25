test_that("Simple power works without overdispersion", {
  p <- relR_power(665, 1.5, 2, 0.5, 1000)
  expect_equal(round(p, 1), 0.8)
})

test_that("Simple power works with overdispersion", {
  p <- relR_power(983, 1.5, 2, 0.5, 1000, overdispersion = 350)
  expect_equal(round(p, 1), 0.8)
})
