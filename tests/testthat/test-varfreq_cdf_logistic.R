
test_that("input arguments are valid", {

    expect_error(varfreq_cdf_logistic(t = "invalid", p0_v1 = 0.1, r_v1 = 0.1, c_ratio = 1))

    expect_error(varfreq_cdf_logistic(t = 5, p0_v1 = 1.1, r_v1 = 0.1, c_ratio = 1))

    expect_error(varfreq_cdf_logistic(t = 5, p0_v1 = "invalid", r_v1 = 0.1, c_ratio = 1))

    expect_error(varfreq_cdf_logistic(t = 5, p0_v1 = 0.1, r_v1 = "invalid", c_ratio = 1))

    expect_error(varfreq_cdf_logistic(t = 5, p0_v1 = 0.1, r_v1 = 0, c_ratio = 1))

    expect_error(varfreq_cdf_logistic(t = 5, p0_v1 = 0.1, r_v1 = 0.1, c_ratio = "invalid"))

    expect_error(varfreq_cdf_logistic(t = 5, p0_v1 = 0.1, r_v1 = 0.1, c_ratio = 0))
})

test_that("return object is valid double", {

    expect_type(varfreq_cdf_logistic(t = 5, p0_v1 = 0.1, r_v1 = 0.1, c_ratio = 1),
        "double")
})

test_that("cdf increases monotonically", {

    expect_gt(varfreq_cdf_logistic(t = 5, p0_v1 = 0.1, r_v1 = 0.1, c_ratio = 1),
        varfreq_cdf_logistic(t = 3, p0_v1 = 0.1, r_v1 = 0.1, c_ratio = 1))

    expect_lt(varfreq_cdf_logistic(t = 5, p0_v1 = 0.1, r_v1 = 0.1, c_ratio = 1.5),
        varfreq_cdf_logistic(t = 7, p0_v1 = 0.1, r_v1 = 0.1, c_ratio = 1.5))

})

test_that("manuscript values remain valid", {

    expect_equal(round(varfreq_cdf_logistic(t = 14, p0_v1 = 1/10000, r_v1 = 0.1,
        c_ratio = 1) - varfreq_cdf_logistic(t = 0, p0_v1 = 1/10000, r_v1 = 0.1, c_ratio = 1),
        8), 0.00305473)

})
