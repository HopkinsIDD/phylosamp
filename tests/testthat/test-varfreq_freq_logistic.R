
test_that("input arguments are valid", {

    expect_error(varfreq_freq_logistic(t = "invalid", p0_v1 = 0.1, r_v1 = 0.1, c_ratio = 1))

    expect_error(varfreq_freq_logistic(t = 5, p0_v1 = 1.1, r_v1 = 0.1, c_ratio = 1))

    expect_error(varfreq_freq_logistic(t = 5, p0_v1 = "invalid", r_v1 = 0.1, c_ratio = 1))

    expect_error(varfreq_freq_logistic(t = 5, p0_v1 = 0.1, r_v1 = "invalid", c_ratio = 1))

    expect_error(varfreq_freq_logistic(t = 5, p0_v1 = 0.1, r_v1 = 0, c_ratio = 1))

    expect_error(varfreq_freq_logistic(t = 5, p0_v1 = 0.1, r_v1 = 0.1, c_ratio = "invalid"))

    expect_error(varfreq_freq_logistic(t = 5, p0_v1 = 0.1, r_v1 = 0.1, c_ratio = 0))
})

test_that("return object is valid double", {

    expect_type(varfreq_freq_logistic(t = 5, p0_v1 = 0.1, r_v1 = 0.1, c_ratio = 1),
        "double")
})

test_that("variant prevalence increases when growth rate is positive", {

    expect_gt(varfreq_freq_logistic(t = 5, p0_v1 = 1/10000, r_v1 = 0.3, c_ratio = 1),
        1/10000)
})

test_that("variant prevalence decreases when growth rate is negative", {

    expect_lt(varfreq_freq_logistic(t = 5, p0_v1 = 1/10000, r_v1 = -0.3, c_ratio = 1),
        1/10000)
})

test_that("observed variant prevalence is higher when c_ratio is above 1", {

    expect_gt(varfreq_freq_logistic(t = 4, p0_v1 = 1/10000, r_v1 = 0.2, c_ratio = 1.2),
        varfreq_freq_logistic(t = 4, p0_v1 = 1/10000, r_v1 = 0.2, c_ratio = 1))
})

test_that("observed variant prevalence is lower when c_ratio is below 1", {

    expect_lt(varfreq_freq_logistic(t = 10, p0_v1 = 1/1000, r_v1 = 0.11, c_ratio = 0.9),
        varfreq_freq_logistic(t = 10, p0_v1 = 1/1000, r_v1 = 0.11, c_ratio = 1))
})

test_that("manuscript results remain valid", {

    expect_equal(round(varfreq_freq_logistic(t = 14, p0_v1 = 1/10000, r_v1 = 0.1,
        c_ratio = 1), 4), 4e-04)
})

